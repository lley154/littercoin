import { useState } from 'react'

const BurnLC = ({ onBurnLC }) => {

    const [qty, setQty] = useState('')

    const onSubmit = (e) => {
        
        e.preventDefault() // prevent full page refresh
        onBurnLC( qty )
    }
    
    return (

        <form onSubmit={onSubmit}>
            <div>
                <b>Littercoin Amount To Burn</b> &nbsp;&nbsp;
                <br></br>
                <input name='qty' type='text' id='qty' placeholder='Enter Amount of Littercoin To Burn' 
                value={qty}
                onChange={(e) => setQty(e.target.value)}
                />
            </div>
            <br/>                      
            <input type='submit' value='Burn Littercoin'/>
        </form>
    )
}

export default BurnLC