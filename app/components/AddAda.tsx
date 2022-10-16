import { useState } from 'react'

const AddAda = ({ onAddAda }) => {

    const [qty, setQty] = useState('')

    const onSubmit = (e) => {
        
        e.preventDefault() // prevent full page refresh
        onAddAda(qty)
    }
    

    return (

        <form onSubmit={onSubmit}>
            <div>
                <b>Add Lovelace (Ada) to the Littercoin Smart Contract</b> &nbsp;&nbsp;
                <br></br>
                <input name='qty' type='text' id='qty' placeholder='Enter Amount of Lovelace to add' 
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