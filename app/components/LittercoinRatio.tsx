const LittercoinRatio = ({ littercoinRatio }) => {

    return (
        <div>
            <p><b>Lovelace / Littercoin Ratio</b> &nbsp; {littercoinRatio.toLocaleString()}</p>
        </div>
    )
}

export default LittercoinRatio