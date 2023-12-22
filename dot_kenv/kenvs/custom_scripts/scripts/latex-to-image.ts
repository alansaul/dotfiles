// Name: Latex to Image

import "@johnlindquist/kit"
import { Action, Choice } from "@johnlindquist/kit"

// I need to downgrade this to sharp@0.32.6 for linux as 0.33 seems to be broken
import sharp from 'sharp'

import { mathjax } from 'mathjax-full/js/mathjax.js'
import { TeX } from 'mathjax-full/js/input/tex.js'
import { SVG } from 'mathjax-full/js/output/svg.js'
import { AllPackages } from 'mathjax-full/js/input/tex/AllPackages.js'
import { liteAdaptor } from 'mathjax-full/js/adaptors/liteAdaptor.js'
import { RegisterHTMLHandler } from 'mathjax-full/js/handlers/html.js'

const defaultImageHeight = 60
let db = await store('latex', {
  imageHeight: defaultImageHeight,
  history: [],
})
let imageHeight: number = await db.get("imageHeight") as number
imageHeight = imageHeight ? imageHeight : defaultImageHeight

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

let sharpOptions = {
  density: 300,
}

const resizedSvgToSharp = async (
  p: string | Buffer,
  { width, height }: { width?: number; height?: number },
  options: sharp.SharpOptions,
) => {
  // resize svg to image buffer, keeping the same pixel density
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


const pngBufferToClipboard = async (pngBuffer: Buffer) => {
  await clipboard.writeImage(pngBuffer)
}


const latexToPngBuffer = async (latex: string): Promise<Buffer> => {
  const node = mathjax_document.convert(latex, mathjax_options)
  const svg = `${adaptor.innerHTML(node)}`
  const sharpBuffer = await resizedSvgToSharp(
    svg, { height: imageHeight }, sharpOptions,
  )
  const pngBuffer = await sharpBuffer.png().toBuffer()
  return pngBuffer
}


let latexToPng = async (latex: string): Promise<string> => {
  const pngBuffer = await latexToPngBuffer(latex)
  const b64 = pngBuffer.toString('base64')
  const encoded = 'data:image/png;base64,' + b64;
  const img = `<img src="${encoded}" />`
  return img
}


let preview = async (input: string): Promise<string> => {
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



const reloadLatexHistory = async (): Promise<string[]> => {
  let latexHistory = await db.get("history") as string[]
  latexHistory.reverse()
  return latexHistory
}


const reloadChoices = async (): Promise<Choice<any>[]> => {
  const latexHistory = await reloadLatexHistory()
  let choices = latexHistory.map((latex: string) => {
    return {
      name: latex,
      preview: async () => {
        return await preview(latex)
      },
      value: latex
    }
  })
  return choices
}


const addToLatexHistory = async (input: string) => {
  if (!input) {
    return
  }
  let latexHistory = await reloadLatexHistory()
  latexHistory?.push(input)
  await db.set('history', latexHistory)
}


let actions: Action[] = [
  {
    shortcut: `${cmd}+c`,
    name: "Copy",
    visible: true,
    onAction: async (input, state) => {
      const selectedValue = state?.focused?.value
      const pngBuffer = await latexToPngBuffer(selectedValue)
      await pngBufferToClipboard(pngBuffer)
    },
  },
  {
    shortcut: `${cmd}+e`,
    name: "Edit",
    // visible: true,
    onAction: async (input, state) => {
      const selectedValue = state?.focused?.value
      setInput(selectedValue ? selectedValue : "")
    },
  },
  {
    shortcut: `${cmd}+d`,
    name: "Delete",
    // visible: true,
    onAction: async (input, state) => {
      const latexHistory = await reloadLatexHistory()
      let filteredLatexHistory = latexHistory.filter((v: string) => {
        return v !== state?.focused?.value
      })
      filteredLatexHistory.reverse()  // reverse it back into time ordered
      await db.set('history', filteredLatexHistory)
      setInput("")
      setChoices(await reloadChoices())
    }
  },
  {
    shortcut: `${cmd}+s`,
    name: "Save",
    visible: true,
    onAction: async (input, state) => {
      await addToLatexHistory(input)
      setInput("")
      setChoices(await reloadChoices())
    }
  },
]


// let choices = Object.assign(reloadChoices, {
//   preload: false
// });
// let choices = async (): Promise<Choice<any>[]> => {
//   let loadedChoices = await reloadChoices()
//   setChoices(loadedChoices)
//   return loadedChoices
// }

let createAndLoadLatex = async () => {
  while (true) {
    let choices = await reloadChoices()
    await arg({
      name: "Latex To Image",
      height: 150,
      input: arg?.input || "",
      placeholder: "Latex to convert to image",
      preventCollapse: true,
      onNoChoices: async (input) => {
        setPanel(await preview(input ? input : ""));
      },
      onSubmit: async (input, state) => {
        await addToLatexHistory(input)
        setInput("")
        setChoices(await reloadChoices())
      }
    }, choices, actions)
  }
}

let changeSettings = async () => {
  while (true) {
    const [height] = await fields({
      fields: [{ label: "Image Height", value: imageHeight.toString(), min: 0 }],
      onSubmit: async (input, state) => {
        // To set the tab, UI still needs to be available, so do it on submit
        const height = state.value?.["0"]
        const parsedHeight = parseInt(height)
        if (!Number.isNaN(parsedHeight)) {
          imageHeight = parsedHeight
          await db.set("imageHeight", imageHeight)
        }
        setTab("Latex")
      },
      onEscape: async (input, state) => {
        setTab("Latex")
      }
    })
  }
}

onTab("Latex", createAndLoadLatex)
onTab("Config", changeSettings)