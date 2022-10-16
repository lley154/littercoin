import { useState, useEffect } from 'react'

const MintNFT = ({ onMintNFT }) => {

    const [address, setAddress] = useState('')

    const onSubmit = (e) => {
        
        e.preventDefault() // prevent full page refresh
        onMintNFT({ address })
    }

    return (

        <form onSubmit={onSubmit}>
            <div>
                <b>NFT Wallet Address</b> &nbsp;&nbsp;
                <br></br>
                <input name='address' type='text' id='address' placeholder='Enter NFT Wallet Address' 
                value={address}
                onChange={(e) => setAddress(e.target.value)}
                />
            </div>
            <br/>                      
            <input type='submit' value='Mint Merchant NFT'/>
        </form>
    )
}

export default MintNFT