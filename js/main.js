main();

async function main() {
    const storage = require('node-persist');
    await storage.init();
    const session = await storage.getItem('session');
    console.log("restore session: " + JSON.stringify(session));


    // Create a new Garmin Connect Client
    const { GarminConnect } = require('garmin-connect');
    const GCClient = new GarminConnect();

    GCClient.restore(session);

    GCClient.onSessionChange(async session => {
        console.log("on session change")
        await storage.setItem('session', session)
    });

}
