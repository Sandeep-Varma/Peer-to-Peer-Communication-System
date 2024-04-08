const { app, BrowserWindow, ipcMain } = require('electron')
const path = require('node:path')

function createWindow(){
    const win = new BrowserWindow({
        width: 800,
        height: 600,
        webPreferences: {
            preload: path.join(__dirname, 'preload.js')
        }
    })
    win.webContents.openDevTools()
    // load renderer.js
    win.loadFile('index.html')
}

app.whenReady().then(() => {
    createWindow()
    app.on('activate', () => {
        if (BrowserWindow.getAllWindows().length === 0)
            createWindow()
    })
})

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit();
})

ipcMain.on('open-chat-window', (event, name) => {
    let chatWindow = new BrowserWindow({
        width: 800,
        height: 600,
        webPreferences: {
            nodeIntegration: true,
            contextIsolation: false
        }
    });

    // Load the chat page for the specific user
    chatWindow.loadFile('chat.html');


    chatWindow.on('closed', () => {
        chatWindow = null;
    });
});