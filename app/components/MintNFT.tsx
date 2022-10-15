import { useState, useEffect } from 'react'

const MintNFT = ({ onMintNFT, nftAddress }) => {

    const [address, setAddress] = useState('')

    const onSubmit = (e) => {
        // prevent full page refresh
        e.preventDefault()
        
        onMintNFT({ address })
        //setAddress(nftAddress)
    }

    /*
    useEffect(() => {
        const getWalletAddress = async () => {
            setAddress(nftAddress)
        }

        getWalletAddress()
    }, [nftAddress])   
    */

    return (

        <form onSubmit={onSubmit}>
            <div>
                <label>NFT Wallet Address &nbsp;&nbsp;</label>
                <input name='address' type='text' id='address' placeholder='Enter NFT Wallet Address' 
                value={address}
                onChange={(e) => setAddress(e.target.value)}
                />
            </div>                      
            <input type='submit' value='Mint Merchant NFT'/>
        </form>


    )
}

export default MintNFT