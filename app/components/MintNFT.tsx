import { useState } from 'react'

const MintNFT = ({ onMintNFT }) => {

    const [address, setAddress] = useState('')

    const onSubmit = (e) => {
        
        e.preventDefault() // prevent full page refresh
        onMintNFT( address )
    }

    return (

        <form onSubmit={onSubmit}>
            <div>
                <b>Merchant Wallet Address</b> 
                <br></br>
                <input name='address' type='text' id='address' placeholder='Enter Merchant Wallet Address' 
                value={address}
                onChange={(e) => setAddress(e.target.value)}
                />
            </div>
            <br></br>                     
            <input type='submit' value='Mint Merchant NFT'/>
        </form>
    )
}

export default MintNFT