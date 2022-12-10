import { Application, Router, Status } from "https://deno.land/x/oak@v10.2.0/mod.ts";

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


const router = new Router();


// Get the utxo(s) for a specific TxId
const hasUtxo = async (txId : string) => {

  const api_key : string = Deno.env.get("NEXT_PUBLIC_BLOCKFROST_API_KEY");
  const blockfrost_url = Deno.env.get("NEXT_PUBLIC_BLOCKFROST_URL");
  const url = blockfrost_url + "/txs/" + txId + "/utxos";

  console.log("hasUtxo:api_key", api_key);
  console.log("hasUtxo:blockfrost_url", blockfrost_url);
  console.log("hasUtxo: url", url);
  
  let resp = await fetch(url, {
    method: "GET",
    headers: {
      accept: "application/json",
      project_id: api_key,
    },
  });

  const payload = await resp.json();
  console.log("payload", payload);
  return payload;
}


// The /api/time endpoint returns the current time in ISO format.
router.get("/api/has-utxo", async (ctx) => {

  const url = new URL(ctx.request.url);
  const txid = url.searchParams.get('txid') as string;
  console.log("router.get: url", url);
  console.log("router.get: txid", txid);

  console.log("payload", hasUtxo(txid));
  const payload = await hasUtxo(txid);

  ctx.response.body = payload;
  ctx.response.type = "json";
  ctx.response.status = Status.OK;

});

// After creating the router, we can add it to the app.
app.use(router.routes());
app.use(router.allowedMethods());

await app.listen({ port: 8000 });
