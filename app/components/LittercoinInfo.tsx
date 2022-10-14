

const LittercoinInfo = ({ littercoinInfo}) => {
    return (
        <div>
            <p><b>Total Lovelace:</b> &nbsp; {littercoinInfo.adaAmount.toLocaleString()}</p>
            <p><b>Total Littercoin:</b> &nbsp; {littercoinInfo.lcAmount.toLocaleString()}</p>
            
        </div>
    )
}

export default LittercoinInfo