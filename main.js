'use strict'
const {app, Tray, Menu, powerSaveBlocker, BrowserWindow} = require('electron');
const path = require('path');

// const app = electron.app
// const BrowserWindow = electron.BrowserWindow

let appIcon;
let mainWindow;
let blocker_id;
const iconPath = path.join(__dirname, 'images', 'clock.png');

app.on('ready', initTray)

function createWindow() {
  mainWindow = new BrowserWindow({width: 1024, height: 768});
  mainWindow.loadURL(`file://${ __dirname }/index.html`)
  //mainWindow.webContents.openDevTools()
  mainWindow.on('closed', function () {
    if (blocker_id) powerSaveBlocker.stop(blocker_id);
    mainWindow = null;
  })
}

function initTray () {
  blocker_id = null;
  appIcon = new Tray(iconPath);
  createWindow();

  var contextMenu = Menu.buildFromTemplate([
    {
      label: 'Multi-timer',
      icon: iconPath,
      click: function() {
        if(!mainWindow) createWindow();
        if (blocker_id) powerSaveBlocker.stop(blocker_id);
        blocker_id = powerSaveBlocker.start('prevent-display-sleep');
      }
    },
    { label: 'Quit',
      accelerator: 'Command+Q',
      selector: 'terminate:',
      click: function() {
        if (blocker_id) powerSaveBlocker.stop(blocker_id);
        if (mainWindow) mainWindow.hide();
        mainWindow = null;
      }
    }
  ]);

  appIcon.setContextMenu(contextMenu);
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
