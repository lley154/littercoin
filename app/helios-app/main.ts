import { Application, Router, Status } from "https://deno.land/x/oak@v10.2.0/mod.ts";

const apiKey : string = Deno.env.get("BLOCKFROST_API_KEY");
const blockfrostAPI = Deno.env.get("BLOCKFROST_API");

const app = new Application();

// First we try to serve static files from the _site folder. If that fails, we
// fall through to the router below.
app.use(async (ctx, next) => {
  try {
    await ctx.send({
      root: `${Deno.cwd()}/_site`,
      index: "index.html",
    });
  } catch {
    await next();
  }
});

// Setup oak middleware to handle api routing
const router = new Router();


// Get the utxo(s) for a specific TxId
const hasUtxo = async (txId : string) => {

  const blockfrostUrl = blockfrostAPI + "/txs/" + txId + "/utxos";

  let resp = await fetch(blockfrostUrl, {
    method: "GET",
    headers: {
      accept: "application/json",
      project_id: apiKey,
    },
  });

  const payload = await resp.json();
  return payload;
}

// Get the utxo(s) for a specific address
const getUtxo = async (addr : string) => {

  const blockfrostUrl = blockfrostAPI + "/addresses/" + addr + "/utxos?order=asc";

  let resp = await fetch(blockfrostUrl, {
    method: "GET",
    headers: {
      accept: "application/json",
      project_id: apiKey,
    },
  });

  const payload = await resp.json();
  return payload;
}


// Blockchain request for utxos for a specific transaction
router.get("/api/has-utxo", async (ctx) => {

  const url = new URL(ctx.request.url);
  const txid = url.searchParams.get('txid') as string;
  const payload = await hasUtxo(txid);

  ctx.response.body = payload;
  ctx.response.type = "json";
  ctx.response.status = Status.OK;

});

// Blockchain request for utxos for a specific transaction
router.get("/api/get-utxo", async (ctx) => {

  const url = new URL(ctx.request.url);
  const txid = url.searchParams.get('addr') as string;
  const payload = await getUtxo(txid);

  ctx.response.body = payload;
  ctx.response.type = "json";
  ctx.response.status = Status.OK;

});




// After creating the router, we can add it to the app.
app.use(router.routes());
app.use(router.allowedMethods());

await app.listen({ port: 8000 });
