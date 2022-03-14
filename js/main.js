import dayjs from "dayjs";
import storage from "node-persist";
import {GarminConnect} from 'garmin-connect';

main();


async function main() {
    await storage.init();
    const session = await storage.getItem('session');

    // Create a new Garmin Connect Client
    const GCClient = new GarminConnect();

    GCClient.restore(session);

    GCClient.onSessionChange(async session => {
        await storage.setItem('session', session)
    });

    const startDate = new Date(2022, 2, 10);
    const endDate = new Date(2022, 2, 14);

    let currentDate = dayjs(startDate);

    while (currentDate.isBefore(endDate) || currentDate.isSame(endDate)) {
        currentDate = currentDate.add(1, "days");
        const sleep = await GCClient.getSleep(currentDate.toDate());
        console.log(sleep.id, sleep.deepSleepSeconds)
    }
}
