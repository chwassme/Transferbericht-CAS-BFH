import storage from "node-persist";
import {GarminConnect} from 'garmin-connect';

createSession()

async function createSession() {
    const GCClient = new GarminConnect();
    await GCClient.login();
    const userInfo = await GCClient.getUserInfo();
    console.log("logged in with ".userInfo);

    const session = GCClient.sessionJson;
    await storage.init();
    await storage.setItem('session', session)
}
