

const LittercoinInfo = ({ littercoinInfo}) => {
    return (
        <div>
            <p><b>Total Lovelace:</b> &nbsp; {littercoinInfo.adaAmount.toLocaleString()}</p>
            <p><b>Total Littercoin:</b> &nbsp; {littercoinInfo.lcAmount.toLocaleString()}</p>
            <p><b>Address:</b> &nbsp; <a href={"https://preview.cexplorer.io/address/" + littercoinInfo.address} target="_blank" rel="noopener noreferrer" >{littercoinInfo.address}</a></p>            
        </div>
    )
}

export default LittercoinInfo