// Name: Clockify start or stop time entry

import "@johnlindquist/kit"

let newTimeEntryTask = async () => {
  let API_KEY = await env("CLOCKIFY_API_KEY")
  const headers = {
    headers: {
      "x-api-key": API_KEY,
      "User-Agent": "insomnia/8.4.5",
    }
  }

  let userResponse = await get(
    "https://api.clockify.me/api/v1/user", headers
  )

  const user = userResponse.data

  let workspacesResponse = await get(
    "https://api.clockify.me/api/v1/workspaces",
    headers,
  )

  let workspace
  // Get a list of workspaces, or choose first one if only one
  if (workspacesResponse?.data?.length > 1) {
    workspace = await arg("Select workspace", async () => {
      return workspacesResponse?.data?.map(
        value => {
          return {
            name: value.name,
            //description: value.name,
            value,
          }
        }
      )
    })
  } else {
    workspace = workspacesResponse?.data[0]
  }

  const getOnGoingTimeEntries = async () => {
    // Get all time entries
    let timeEntriesResponse = await get(
      `https://api.clockify.me/api/v1/workspaces/${workspace.id}/user/${user.id}/time-entries`,
      headers,
    )

    // Filter them to only those that have a null end
    const onGoingTimeEntries = timeEntriesResponse.data.filter(
      (item) => {
        return item.timeInterval.end === null
      }
    )
    return onGoingTimeEntries
  }

  // Function (also used as callback in pomodoro timer) to stop current time entries
  const stopEntry = async (endDate) => {
    let stopEntryResponse
    const onGoing = await getOnGoingTimeEntries()
    if (onGoing.length > 0) {
      stopEntryResponse = await patch(`https://api.clockify.me/api/v1/workspaces/${workspace.id}/user/${user.id}/time-entries`,
        { "end": endDate.toISOString() },
        headers
      )
    }
    return stopEntryResponse
  }


  const onGoing = await getOnGoingTimeEntries()
  for (const timeEntry of onGoing) {
    const shouldStopTimeEntry = await arg({
      placeholder: `Stop ongoing time entry:  ${timeEntry.description}`,
      focused: 'Yes',
    }, ['Yes', 'No'])
    if (shouldStopTimeEntry == 'Yes') {
      stopEntry(new Date())
    }
  }


  // Select a project from the list of existing projects, or create a new one
  const project = await arg(
    {
      placeholder: `Select project`,
      strict: false,
    },
    async () => {
      let projectsResponse = await get(
        `https://api.clockify.me/api/v1/workspaces/${workspace.id}/projects`,
        headers
      )
      return projectsResponse?.data?.map(
        value => {
          return {
            name: value.name,
            //description: value.name,
            value,
          }
        }
      )
    }
  )


  // Create new task or continue existing one
  let findTasksUrl = `https://api.clockify.me/api/v1/workspaces/${workspace.id}/projects/${project.id}/tasks`
  const taskOrTaskname = await arg(
    {
      placeholder: `Create or continue task for project`,
      strict: false,
    },
    async () => {
      let tasksResponse = await get(
        findTasksUrl,
        headers
      )
      return tasksResponse?.data?.map(
        (value) => {
          return {
            name: value.name,
            //description: value.name,
            value,
          }
        }
      )
    }
  )

  // May have new task name, or an existing task (with an id)
  let task
  if (typeof taskOrTaskname === 'object' && taskOrTaskname !== null) {
    // Already have a task object
    task = taskOrTaskname
  } else {
    // Create a new one
    let taskResponse = await post(
      `https://api.clockify.me/api/v1/workspaces/${workspace.id}/projects/${project.id}/tasks`,
      {
        projectId: project.id,
        workspaceId: workspace.id,
        name: taskOrTaskname,
      },
      headers
    )
    task = taskResponse.data
  }

  const timeEntryDescription = await arg(
    "Time entry description"
  )

  // Function (also used as callback in pomodoro timer) to start current entry with fixed project id, task id and description
  const startEntry = async (startDateTime) => {
    // Create time entry for given task
    const startDateTimeIsoFormat = startDateTime.toISOString();
    let newTimeEntryResponse = await post(
      `https://api.clockify.me/api/v1/workspaces/${workspace.id}/time-entries`,
      {
        projectId: project.id,
        taskId: task.id,
        description: timeEntryDescription,
        start: startDateTimeIsoFormat,
      },
      headers
    )
    return newTimeEntryResponse
  }

  return [startEntry, stopEntry]
}

// Pomodoro
const pomodoroResponse = await arg({
  placeholder: `Use Pomodoro?`,
  focused: 'Yes',
}, ['Yes', 'No'])
const usePomodoro = pomodoroResponse == 'Yes'

if (!usePomodoro) {
  let [startTimeEntry, stopTimeEntry] = await newTimeEntryTask()
  await startTimeEntry(new Date())
} else {
  let clock = await widget(`
<div><link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta2/css/all.min.css"/>
<link href="https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&display=swap" rel="stylesheet">
<style>
  .bg-dark-grey { background-color: #2C3E50; }
  .text-orange { color: #F5A623; }
  .text-light { color: #EAEAEA; }
  .circle-progress {
    position: relative;
    width: 80px;
    height: 80px;
    border-radius: 50%;
    background: conic-gradient(
      from 0deg,
      #F5A623 0%,
      #F5A623 {{percentageThrough}}%,
      #494D4A {{percentageThrough}}%,
      #494D4A 100%
    );
  }
  .circle-progress .timer {
    position: absolute;
    width: 70px;
    height: 70px;
    top: 5px;
    left: 5px;
    border-radius: 50%;
    background-color: #2C3E50;
    display: flex;
    align-items: center;
    justify-content: center;
    flex-direction: column;
  }
</style>
<div class="bg-dark-grey text-light min-h-screen flex flex-col items-center justify-center p-8">
      <div class="flex items-center my-1">
          <span class="text-orange font-medium uppercase text-sm">{{periodType}}</span>
    </div>
  <div class="flex flex-col items-center my-2">
    <div class="circle-progress">
      <div class="timer">
        <span class="text-lg font-semibold">{{timeRemaining}}</span>
        <span class="text-sm text-orange font-semibold">{{pomodoros}}</span>
      </div>
    </div>

    <div class="flex space-x-6 items-center my-3">
      <button id="start" class="text-orange text-l">
        <i class="fas fa-play"></i>
      </button>
      <button id="pause" class="text-orange text-l">
        <i class="fas fa-pause"></i>
      </button>
      <button id="reset" class="text-orange text-l">
        <i class="fas fa-refresh"></i>
      </button>
      <button id="next" class="text-orange text-l">
        <i class="fas fa-forward"></i>
      </button>
      <button id="stop" class="text-orange text-l">
        <i class="fas fa-stop"></i>
      </button>
      <button id="change" class="text-orange text-l">
        <i class="fas fa-right-left"></i>
      </button>
    </div>
  </div>
</div>
</div>
`,
    {
      width: 250,
      height: 170,
      transparent: true,
      draggable: true,
      hasShadow: false,
      alwaysOnTop: true,
    })

  // interface Sprint {
  //   periodType: string;
  //   counterMinutes: number;
  // }

  // Format seconds as mm*:ss or -mm*:ss if we are negative seconds
  // TODO: tidy
  const formatSeconds = (seconds) => {
    const sign = seconds < 0 ? "-" : "";
    seconds = Math.abs(seconds);
    const h = Math.floor(seconds / 3600);
    let m = Math.floor((seconds % 3600) / 60);
    const s = Math.floor(seconds % 60);
    if (h > 0) {
      m = m.toString().padStart(2, "0");
      return sign + h + ":" + m + ":" + (s < 10 ? '0' : '') + s;
    } else {
      return sign + (m < 10 ? '0' : '') + m + ":" + (s < 10 ? '0' : '') + s;
    }
  }

  let currentTimer

  // Update clockface
  const setClockState = (secondsRemaining, totalSeconds) => {
    let percentageThrough = Math.min(100 - (secondsRemaining / totalSeconds) * 100, 100)
    let timeRemaining = formatSeconds(secondsRemaining)
    clock.setState({
      timeRemaining: timeRemaining,
      percentageThrough: percentageThrough,
    })
  }

  // New chunk of work (or break)
  let newSession = (session, startCallback, stopCallback) => {
    return new Promise((resolve, reject) => {
      const totalSeconds = session.counterMinutes * 60
      let secondsRemaining = totalSeconds
      let now
      let endTime
      let isPaused = false

      clock.setState({ periodType: session.periodType })
      setClockState(secondsRemaining, totalSeconds)

      let currentInterval

      let timerStop = async (triggerCallback = true) => {
        if (currentInterval !== null) {
          clearInterval(currentInterval)
          currentInterval = null
        }
        if (triggerCallback) {
          await stopCallback(new Date())
        }
      }

      let timer = {}
      timer._step = async () => {
        currentInterval = setInterval(async () => {
          now = new Date()
          secondsRemaining = (endTime.getTime() - now.getTime()) / 1000;

          setClockState(secondsRemaining, totalSeconds)
          if (secondsRemaining <= 0) {
            await currentTimer.finished()
          }
        }, 1000)
      }
      timer.start = async () => {
        await timerStop()
        clock.setState({ periodType: session.periodType })
        isPaused = false
        now = new Date()
        endTime = new Date(now.getTime() + secondsRemaining * 1000)
        await startCallback(now)
        currentTimer._step(secondsRemaining)
      }
      timer.pause = async () => {
        await timerStop()
        clock.setState({ periodType: "Paused" })
        isPaused = true
      }
      timer.reset = async () => {
        await currentTimer.pause()
        secondsRemaining = totalSeconds
        await currentTimer.start()
      }
      timer.next = async () => {
        await timerStop()
        clock.setState({ periodType: "Skipped" })
        setClockState(secondsRemaining, totalSeconds)
        resolve("skipped")
      }
      timer.finished = async () => {
        // When a session has finished, carry on 
        // IMO f you're in the zone you shouldn't be forced to stop or lose logged time
        clock.setState({
          periodType: "Finished",
        })
        // notify("Chunk finished")
      }
      timer.stop = async () => {
        clock.setState({ periodType: "Stopped" })
        await timerStop()
        secondsRemaining = 0
        setClockState(secondsRemaining, totalSeconds)
        resolve("stop")
      }
      timer.change = async () => {
        clock.setState({ periodType: "Change" })
        await timerStop()
        secondsRemaining = 0
        setClockState(secondsRemaining, totalSeconds)
        resolve("change")
      }
      //timer.start()
      currentTimer = timer
    })
  }


  clock.onClick(async (event) => {
    if (event.targetId === "start") {
      currentTimer.start()
    }
    if (event.targetId === "pause") {
      currentTimer.pause()
    }
    if (event.targetId === "reset") {
      currentTimer.reset()
    }
    if (event.targetId === "next") {
      currentTimer.next()
    }
    if (event.targetId === "stop") {
      currentTimer.stop()
    }
    if (event.targetId === "change") {
      currentTimer.change()
    }
  })

  clock.onClose(async () => {
    await currentTimer.stop()
  })

  let work = {
    periodType: "Work",
    counterMinutes: 25,
  }

  let shortBreak = {
    periodType: "Short break",
    counterMinutes: 5,
  }

  let longBreak = {
    periodType: "Long break",
    counterMinutes: 15,
  }

  let currentSession = work
  let repsBeforeLongBreak = 3

  let repsDone = 0
  let pomodoros = -1
  const increasePomodoroCount = () => {
    pomodoros = pomodoros + 1
    clock.setState({pomodoros: pomodoros})
  }
  increasePomodoroCount()
  const nullCallback = async () => { }

  const runPomodoroTimer = async () => {
    // ((Work -> shortbreak)*numberOfSessions -> longBreak)*infinite
    let sessionResponse
    // New type of task, get callbacks to start and stop it
    let [startTimeEntry, stopTimeEntry] = await newTimeEntryTask()
    // Move to a background process (?)
    hide()
    while (true) {
      while (repsDone < repsBeforeLongBreak) {
        currentSession = work
        sessionResponse = await newSession(currentSession, startTimeEntry, stopTimeEntry)
        if (sessionResponse === 'stop' || sessionResponse == 'change') return sessionResponse
        increasePomodoroCount()
        currentSession = shortBreak
        sessionResponse = await newSession(currentSession, nullCallback, nullCallback)
        if (sessionResponse === 'stop' || sessionResponse == 'change') return sessionResponse
        repsDone = repsDone + 1
      }
      currentSession = longBreak
      sessionResponse = await newSession(currentSession, nullCallback, nullCallback)
      if (sessionResponse === 'stop' || sessionResponse == 'change') return sessionResponse
      repsDone = 0
    }
  }

  let sessionResponse
  while (sessionResponse !== 'stop') {
    sessionResponse = await runPomodoroTimer()
  }
  clock.close()
}