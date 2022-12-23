const LittercoinInfo = ({ littercoinInfo } : any) => {

    var ratio : BigInt = BigInt(0);
    const lc : BigInt = BigInt(littercoinInfo.lcAmount);
    const ada : BigInt = BigInt(littercoinInfo.adaAmount);
    if (Number(lc) > 0 ) {
        ratio = BigInt(littercoinInfo.adaAmount) / BigInt(littercoinInfo.lcAmount);
    }
    
    return (
        <div>
            <p><b>Total Lovelace</b> &nbsp; {Number(ada).toLocaleString()}</p>
            <p><b>Total Littercoin</b> &nbsp; {Number(lc).toLocaleString()}</p>
            <p><b>Lovelace / Littercoin Ratio</b> &nbsp; {Number(ratio).toLocaleString()}</p>
            <p><b>Address:</b> &nbsp; <a href={"https://preprod.cexplorer.io/address/" + littercoinInfo.address} target="_blank" rel="noopener noreferrer" >{littercoinInfo.address}</a></p>            
        </div>
    )
}

export default LittercoinInfo