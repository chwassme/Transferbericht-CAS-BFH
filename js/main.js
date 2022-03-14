import dayjs from "dayjs";
import storage from "node-persist";
import fs from "fs";
import {GarminConnect} from 'garmin-connect';
import {toDateString} from "garmin-connect/dist/common/DateUtils.js";

main();

async function main() {
    await storage.init();
    const session = await storage.getItem('session');

    // Create a new Garmin Connect Client
    const GCClient = new GarminConnect2();

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

    const stepsKeys = ['startGMT', 'endGMT', 'steps', 'primaryActivityLevel', 'activityLevelConstant'];
    const stepsData = [stepsKeys.join(';')];

    const stressKeys = ['calendarDate', 'maxStressLevel', 'avgStressLevel'];
    const stressData = [stressKeys.join(';')];

    const stressValuesKeys = ['timestamp', 'stressLevel'];
    const stressValues = [stressValuesKeys.join(';')];

    while (currentDate.isBefore(endDate) || currentDate.isSame(endDate)) {
        const curDate = currentDate.toDate();

        const stress = await GCClient.getStress(curDate);
        stressData.push(stressKeys.map(key => stress[key]).join(';'));
        stressValues.push(...stress.stressValuesArray.map(value => value.join(';')));

        const sleep = await GCClient.getSleep(curDate);
        if (!sleepKeys) {
            sleepKeys = Object.keys(sleep).join(';');
            sleepData.push(sleepKeys);
        }
        sleepData.push(Object.keys(sleep).map(key => sleep[key]).join(';'));

        const heartRate = await GCClient.getHeartRate(curDate);
        heartRateData.push(heartRateKeys.map(key => heartRate[key]).join(';'));

        const steps = await GCClient.getSteps(curDate);
        steps.forEach(stepData => {
            stepsData.push(stepsKeys.map(key => stepData[key]).join(';'));
        });


        console.log(curDate, sleep.id, sleep.deepSleepSeconds, steps.length, heartRate.restingHeartRate)

        currentDate = currentDate.add(1, "days");
    }
    writeFile('sleep', sleepData);
    writeFile('heartrate', heartRateData);
    writeFile('steps', stepsData);
    writeFile('stress', stressData);
    writeFile('stressvalues', stressValues);
}

function writeFile(name, data) {
    const fileName = 'data/' + name + '.csv';
    fs.writeFileSync(fileName, data.join('\n'), {flag: 'w'}, err => {
        console.error(err)
    });
}

class GarminConnect2 extends GarminConnect {

    async getStress(date = new Date()) {
        const dateString = toDateString(date);
        const url = 'https://connect.garmin.com/modern/proxy/wellness-service/wellness/dailyStress/' + dateString;
        return this.get(url, { date: dateString });
    }
}

function getStress() {
    // https://connect.garmin.com/modern/proxy/wellness-service/wellness/dailyHeartRate/chregi23
    // https://connect.garmin.com/modern/proxy/wellness-service/wellness/dailyStress/2022-03-13
}
