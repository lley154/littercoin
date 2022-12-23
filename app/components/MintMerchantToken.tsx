import { useState } from 'react'

const MintMerchantToken = ({ onMintMerchantToken } : any) => {

    const [address, setAddress] = useState('')

    const onSubmit = (e : any) => {
        
        e.preventDefault() // prevent full page refresh
        onMintMerchantToken( address )
    }

    return (

        <form onSubmit={onSubmit}>
            <div>
                <b>Mint Merchant Token</b> 
                <br></br>
                <input name='address' type='text' id='address' placeholder='Enter Merchant Wallet Address' 
                value={address}
                onChange={(e) => setAddress(e.target.value)}
                />
            </div>
            <br></br>                     
            <input type='submit' value='Mint Merchant Token'/>
        </form>
    )
}

export default MintMerchantToken