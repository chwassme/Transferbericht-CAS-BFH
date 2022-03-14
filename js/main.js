main();


import dayjs from "dayjs";
import storage from "node-persist";
import { GarminConnect } from 'garmin-connect';


async function main() {
    await storage.init();
    const session = await storage.getItem('session');

    // Create a new Garmin Connect Client
    const GCClient = new GarminConnect();

    GCClient.restore(session);

    GCClient.onSessionChange(async session => {
        await storage.setItem('session', session)
    });

    dayjs('2018-08-08');

    const data = await GCClient.getSleep();
    console.log(data)

}
