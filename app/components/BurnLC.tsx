import { useState } from 'react'

const BurnLC = ({ onBurnLC } : any) => {

    const [qty, setQty] = useState('')

    const onSubmit = (e : any) => {
        
        e.preventDefault() // prevent full page refresh
        onBurnLC( qty )
    }
    
    return (

        <form onSubmit={onSubmit}>
            <div>
                <b>Littercoin Amount To Burn</b> 
                <br></br>
                <input name='qty' type='number' id='qty' placeholder='Enter Amount of Littercoin To Burn' 
                value={qty}
                onChange={(e) => setQty(e.target.value)}
                />
            </div>
            <br></br>                    
            <input type='submit' value='Burn Littercoin'/>
        </form>
    )
}

export default BurnLC