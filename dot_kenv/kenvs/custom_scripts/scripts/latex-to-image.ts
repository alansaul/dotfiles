// Name: Latex to Image

import "@johnlindquist/kit"
import { Action } from "@johnlindquist/kit"
import { Channel } from "@johnlindquist/kit/core/enum"
import { ChannelHandler } from "@johnlindquist/kit/types/core"

//import _ from "lodash"

// I need to downgrade this to sharp@0.32.6 for linux as 0.33 seems to be broken
import sharp from 'sharp'

import { mathjax } from 'mathjax-full/js/mathjax.js'
import { TeX } from 'mathjax-full/js/input/tex.js'
import { SVG } from 'mathjax-full/js/output/svg.js'
import { AllPackages } from 'mathjax-full/js/input/tex/AllPackages.js'
import { liteAdaptor } from 'mathjax-full/js/adaptors/liteAdaptor.js'
import { RegisterHTMLHandler } from 'mathjax-full/js/handlers/html.js'
import { PromptConfig } from "@johnlindquist/kit"

// import { NativeImage, Clipboard} from "electron"
// import pkg from "electron"
// const { clipboard } = pkg


const adaptor = liteAdaptor()
RegisterHTMLHandler(adaptor)

const mathjax_document = mathjax.document('', {
  InputJax: new TeX({ packages: AllPackages }),
  OutputJax: new SVG({ fontCache: 'local' })
})

const mathjax_options = {
  em: 16,
  ex: 8,
  containerWidth: 1280
}

let imageHeight = 30
let sharpOptions = {
    density: 72,
}

let base64ToBlob = (base64: string) => {
  const byteCharacters = atob(base64);
  const byteNumbers = new Array(byteCharacters.length);
  for (let i = 0; i < byteCharacters.length; i++) {
    byteNumbers[i] = byteCharacters.charCodeAt(i);
  }
  const byteArray = new Uint8Array(byteNumbers);
  return new Blob([byteArray], { type: 'image/png' });
}

const resizedSvgToSharp = async (
  p: string | Buffer,
  { width, height }: { width?: number; height?: number },
  options: sharp.SharpOptions,
) => {
  const instance = sharp(Buffer.from(p), options)

  const metadata = await instance.metadata()

  const initDensity = metadata.density ?? 72

  if (metadata.format !== 'svg') {
    return instance
  }

  let wDensity = 0
  let hDensity = 0
  if (width && metadata.width) {
    wDensity = (initDensity * width) / metadata.width
  }

  if (height && metadata.height) {
    hDensity = (initDensity * height) / metadata.height
  }

  if (!wDensity && !hDensity) {
    // both width & height are not present and/or
    // can't detect both metadata.width & metadata.height
    return instance
  }

  return sharp(Buffer.from(p), { density: Math.max(wDensity, hDensity) }).resize(
    width,
    height,
  )
}

let latexToPng = async (latex: string) => {
    const node = mathjax_document.convert(latex, mathjax_options)
    const svg = `${adaptor.innerHTML(node)}`

    // let sharpBuffer = await sharp(Buffer.from(svg), sharpOptions)
    //     .resize({height: imageHeight})
    const sharpBuffer = await resizedSvgToSharp(
        svg, {height: imageHeight}, sharpOptions,
    )

    const pngBuffer = await sharpBuffer.png().toBuffer()

    // const imageBuffer = await sharp(Buffer.from(svg))
    //     .resize({height: imageHeight})
    //     .toBuffer()

    const b64 = pngBuffer.toString('base64')
    const blob = base64ToBlob(b64);
    // let image = nativeImage.createFromBuffer(imageBuffer)
    // clipboard.writeText(b64)
    let filePath = tmpPath("latex.png")
    await writeFile(filePath, pngBuffer)
    await sendWait(Channel.CLIPBOARD_WRITE_IMAGE, filePath)
    // await sendWait(Channel.CLIPBOARD_WRITE_TEXT, "bye")
    // await sendWait(Channel.COPY_PATH_AS_PICTURE, {value: blob})
    const encoded = 'data:image/png;base64,' + b64;
    // await copy(encoded)
    const img = `<img src="${encoded}" />`
    return img
}


let preview = async (input: string ) => {
    let innerText = ''
    if (input) {
        try {
            innerText = await latexToPng(input)
        } catch (err) {
            innerText = err.message
        }
    }
    return `<div class="min-w-full min-h-full bg-white"><div class="p-3">${innerText}</div></div>`
}


let db = await store('latex', {
  history: [],
})
  
const reloadLatexHistory = async () => {
  let latexHistory = await db.get("history") as string[]
  latexHistory.reverse()
  return latexHistory
}

const reloadChoices = async () => {
  const latexHistory = await reloadLatexHistory()
  let choices = latexHistory.map((l) => {
    return {
      name: `${l}`,
      preview: async () => {
        return await preview(l)
      },
      value: l
    }
  })
  return choices
}

let choicesOrPanel = await reloadChoices()


let actions: Action[] = [
  // {
  //   shortcut: `${cmd}+c`,
  //   name: "Copy",
  //   visible: true,
  //   onAction: async (input, state) => {
  //     state?.preview  // this contains the img
  //   },
  // },
  // {
  //   name: "Change preview size",
  //   visible: true,
  //   onAction: async (input, state) => {
  //     if (!input){
  //       return
  //     }
  //     const inputAsNumber: number = parseInt(input)
  //     if (inputAsNumber) {
  //       imageHeight = inputAsNumber
  //     }
  //   },
  // },
  {
    shortcut: `${cmd}+e`,
    name: "Edit",
    visible: true,
    onAction: async (input, state) => {
      const selectedValue = state?.focused?.value
      setInput(selectedValue ? selectedValue : "")
      inspect(state)
    },
  },
  {
    shortcut: `${cmd}+d`,
    name: "Delete",
    visible: true,
    onAction: async (input, state) => {
      const latexHistory = await reloadLatexHistory()
      let filteredLatexHistory = latexHistory.filter((v: string) => {
        return v !== state?.focused?.value
      })
      filteredLatexHistory.reverse()  // reverse it back into time ordered
      await db.set('history', filteredLatexHistory)
      setChoices(await reloadChoices())
      setInput("")
    }
  },
  {
    shortcut: `${cmd}+s`,
    name: "Save",
    visible: true,
    onAction: async (input, state) => {
      if (!input) {
        return
      }
      let latexHistory = await reloadLatexHistory()
      latexHistory?.push(input)
      await db.set('history', latexHistory)
      setChoices(await reloadChoices())
      setInput("")
    }
  },
]

sendWait(Channel.CLIPBOARD_WRITE_TEXT, "hello")

await arg({
  name: "Latex To Image",
  height: 150,
  input: arg?.input || "",
  placeholder: "Latex to convert to image",
  preventCollapse: true,
  onNoChoices: async (input) => {
    setPanel(await preview(input ? input : ""));
  },
}, choicesOrPanel, actions)


// Use preview?
// let promptConfig: PromptConfig = {
//     placeholder: "Latex to convert",
//     height: 150,
//     itemHeight: 150,
// }
// await arg(
//     promptConfig, choicesOrPanel//, actionsOrPreview
// )

// let latex = await arg("Latex to convert")
// let img = await latexToPng(latex)

// await div(
//     {
//         resize: true,
//         height: 500,
//         width: 500,
//         html: md(img),
//     },
//     "w-screen bg-white"
// )

// await hide()
// await keyboard.pressKey(Key.LeftSuper, Key.C)

// appside:
// kitMessageMap.COPY_PATH_AS_PICTURE: (data) => {
//   clipboard.writeImage(data.value as any);
// },
// sendWait(Channel.COPY_PATH_AS_PICTURE, )
