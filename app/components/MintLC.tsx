import { useState } from 'react'

const MintLC = ({ onMintLC } : any) => {

    const [address, setAddress] = useState('')
    const [qty, setQty] = useState('')

    const onSubmit = (e : any) => {
        
        e.preventDefault() // prevent full page refresh
        onMintLC([address, qty])
    }
    

    return (

        <form onSubmit={onSubmit}>
            <div>
                <b>User Wallet Address</b> 
                <br></br>
                <input name='address' type='text' id='address' placeholder='Enter User Wallet Address' 
                value={address}
                onChange={(e) => setAddress(e.target.value)}
                />
                <p></p>                 
            </div>
            <div>
                <b>Littercoin Amount To Mint</b> 
                <br></br>
                <input name='qty' type='number' id='qty' placeholder='Enter Amount of Littercoin To Mint' 
                value={qty}
                onChange={(e) => setQty(e.target.value)}
                />
            </div>
            <br></br>                   
            <input type='submit' value='Mint Littercoin'/>
        </form>
    )
}

export default MintLC