createSession()

async function createSession() {
    const {GarminConnect} = require('garmin-connect');
    const GCClient = new GarminConnect();
    await GCClient.login();
    const userInfo = await GCClient.getUserInfo();
    console.log("logged in with " . userInfo);

    const session = GCClient.sessionJson;
    const storage = require('node-persist');
    await storage.init();
    await storage.setItem('session', session)
}
