main();

async function main() {
    const storage = require('node-persist');
    await storage.init();
    const session = await storage.getItem('session');

    // Create a new Garmin Connect Client
    const { GarminConnect } = require('garmin-connect');
    const GCClient = new GarminConnect();

    GCClient.restore(session);

    GCClient.onSessionChange(async session => {
        await storage.setItem('session', session)
    });

    const data = await GCClient.getSleep();
    console.log(data)

}
