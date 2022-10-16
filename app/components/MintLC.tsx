import { useState, useEffect } from 'react'

const MintLC = ({ onMintLC }) => {

    const [address, setAddress] = useState('')
    const [qty, setQty] = useState('')

    const onSubmit = (e) => {
        
        e.preventDefault() // prevent full page refresh
        onMintLC([address, qty])
    }
    

    return (

        <form onSubmit={onSubmit}>
            <div>
                <b>Littercoin Wallet Address</b> &nbsp;&nbsp;
                <br></br>
                <input name='address' type='text' id='address' placeholder='Enter Littercoin Wallet Address' 
                value={address}
                onChange={(e) => setAddress(e.target.value)}
                />
            </div>
            <div>
                <b>Littercoin Amount To Mint</b> &nbsp;&nbsp;
                <br></br>
                <input name='qty' type='text' id='qty' placeholder='Enter Amount of Littercoin To Mint' 
                value={qty}
                onChange={(e) => setQty(e.target.value)}
                />
            </div>
            <br/>                      
            <input type='submit' value='Mint Littercoin'/>
        </form>
    )
}

export default MintLC