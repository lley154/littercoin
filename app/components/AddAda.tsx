import { useState } from 'react'
import styles from '../styles/Home.module.css'

const AddAda = ({ onAddAda } : any) => {

    const [qty, setQty] = useState('')

    const onSubmit = (e : any) => {
        
        e.preventDefault() // prevent full page refresh
        onAddAda(qty)
    }
    

    return (

        <form onSubmit={onSubmit}>
            <div>
                <b>Add Lovelace (Ada) to the Littercoin Smart Contract</b>
                <br></br>
                <input name='qty' type='number' id='qty' placeholder='Enter Amount of Lovelace to add' 
                value={qty}
                onChange={(e) => setQty(e.target.value)}
                />
            </div>
            <br/>                      
            <input type='submit' value='Add Lovelace'/>
        </form>
    )
}

export default AddAda