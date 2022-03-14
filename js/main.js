import dayjs from "dayjs";
import storage from "node-persist";
import fs from "fs";
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

    const startDate = new Date(2021, 4, 20);
    const endDate = new Date(2021, 4, 20);

    let currentDate = dayjs(startDate);
    const sleepData = [];
    let sleepKeys;

    let heartRateKeys = [
        'calendarDate'
        , 'maxHeartRate'
        , 'minHeartRate'
        , 'restingHeartRate'
        , 'lastSevenDaysAvgRestingHeartRate'
    ];
    const heartRateData = [heartRateKeys.join(';')];

    while (currentDate.isBefore(endDate) || currentDate.isSame(endDate)) {
        const curDate = currentDate.toDate();

        const sleep = await GCClient.getSleep(curDate);
        if (!sleepKeys) {
            sleepKeys = Object.keys(sleep).join(';');
            sleepData.push(sleepKeys);
        }
        sleepData.push(Object.keys(sleep).map(key => sleep[key]).join(';'));

        const heartRate = await GCClient.getHeartRate(curDate);
        heartRateData.push(heartRateKeys.map(key => heartRate[key]).join(';'));

        // {
        //   "startGMT": "2022-03-10T07:45:00.0",
        //   "endGMT": "2022-03-10T08:00:00.0",
        //   "steps": 0,
        //   "primaryActivityLevel": "sedentary", // sedentary, sleeping, active, generic, none, highlyActive, ...
        //   "activityLevelConstant": true
        // }
        const steps = await GCClient.getSteps(curDate);


        console.log(curDate, sleep.id, sleep.deepSleepSeconds, steps.length, heartRate.restingHeartRate)

        currentDate = currentDate.add(1, "days");
    }


    fs.writeFileSync('data/sleep.csv', sleepData.join('\n'), {flag: 'w'}, err => {
        console.error(err)
    })

    fs.writeFileSync('data/heartrate.csv', heartRateData.join('\n'), {flag: 'w'}, err => {
        console.error(err)
    })
}
