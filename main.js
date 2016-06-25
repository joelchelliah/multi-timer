'use strict'
const electron = require('electron')

const app = electron.app
const BrowserWindow = electron.BrowserWindow

let mainWindow

app.on('ready', createWindow)

function createWindow () {
  mainWindow = new BrowserWindow({
    width: 1024,
    height: 768
  })

  mainWindow.loadURL(`file://${ __dirname }/index.html`)
  mainWindow.webContents.openDevTools()
  mainWindow.on('closed', function () { mainWindow = null })
}

/* Mac Specific things */

// when you close all the windows on a non-mac OS it quits the app
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') { app.quit() }
})

// if there is no mainWindow it creates one (like when you click the dock icon)
app.on('activate', () => {
  if (mainWindow === null) { createWindow() }
})
