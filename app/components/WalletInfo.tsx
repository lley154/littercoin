const WalletInfo = ({ walletInfo }) => {

    return (
        <div><b>Wallet Balance In Lovelace</b>
            <i>&nbsp;&nbsp;&nbsp;&nbsp;{walletInfo.balance}</i>
        </div>
    )
}

export default WalletInfo

