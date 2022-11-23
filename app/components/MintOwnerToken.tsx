import { useState } from 'react'

const MintOwnerToken = ({ onMintOwnerToken } : any) => {

    const [address, setAddress] = useState('')

    const onSubmit = (e : any) => {
        
        e.preventDefault() // prevent full page refresh
        onMintOwnerToken( address )
    }

    return (

        <form onSubmit={onSubmit}>
            <div>
                <b>Owner Wallet Address</b> 
                <br></br>
                <input name='address' type='text' id='address' placeholder='Enter Owner Wallet Address' 
                value={address}
                onChange={(e) => setAddress(e.target.value)}
                />
            </div>
            <br></br>                     
            <input type='submit' value='Mint Owner Token'/>
        </form>
    )
}

export default MintOwnerToken