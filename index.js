import { Storage } from '@capacitor/storage'
import { App } from '@capacitor/app'
import { Dialog } from '@capacitor/dialog';
import { Browser } from '@capacitor/browser';
import { add, isAfter } from 'date-fns';

const getFromStorage = async key => {
  try {
    return (await Storage.get({ key })).value
  } catch (e) {
    return null
  }
}
const saveToStorage = key => value => Storage.set({ key, value })

const showAlert = (title, message) => {
  Dialog.alert({
    title,
    message
  })
}

// init elm app
const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: ''
})

App.addListener('appUrlOpen', e => {
  // showAlert("code", e.url)
  // showAlert('app ports', JSON.stringify(app))
  const hasCode = e.url.indexOf('?code=') > -1
  if (!hasCode) return
  const start = e.url.indexOf('?code=') + 6
  const end = e.url.indexOf('&scope')
  const code = e.url.slice(start, end)
  // showAlert("code", code)
  app.ports.receiveAuthCode.send(code)
  return
})

app.ports.showAlert.subscribe(async ({ title, message }) => {
  return showAlert(title, message)
})

app.ports.saveToStorage.subscribe(([key, value]) => {
  saveToStorage(key)(value)
})

// login/authorize
app.ports.doLogin.subscribe(async (url) => {
  try {
    await Browser.open({ url })
  } catch (e) {
    Dialog.alert({
      title: 'error',
      message: e.toString()
    })
  }
})

app.ports.checkForRefreshToken.subscribe(async () => {
  // const keys = await Storage.keys()
  // showAlert("keys", JSON.stringify(keys))
  const rt = await getFromStorage('refreshToken')
  return app.ports.receiveRefreshToken.send(rt || '')
})

app.ports.logOut.subscribe(() => {
  Storage.remove({
    key: 'refreshToken'
  })
  App.exitApp()
})

app.ports.getSheetIdFromStorage.subscribe(async () => {
  const sheetId = await getFromStorage('sheetId')
  return app.ports.receiveSheetIdFromStorage.send(sheetId || '')
})

app.ports.exitApp.subscribe(() => App.exitApp())


App.addListener('backButton', () => {
  app.ports.goBack.send('')
})