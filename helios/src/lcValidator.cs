spending validator

struct Datum {
    lcAmount: Int
    adaAmount: Int

    func get_ratio(self) -> Int {
        self.adaAmount / self.lcAmount
    }
}

enum Redeemer {
    AddAda
    Mint 
    Burn
}

const adminPkh : PubKeyHash  = PubKeyHash::new(#b9abcf6867519e28042048aa11207214a52e6d5d3288b752d1c27682)

func main(datum: Datum, redeemer: Redeemer, ctx: ScriptContext) -> Bool {
    redeemer.switch {
        AddAda => {true},
        Mint => {
            ctx.tx.is_signed_by(adminPkh) &&
            datum.get_ratio() == 0
        },
        Burn => {true}

    }
    
}


const MY_DATUM = Datum {
    lcAmount : 0,
    adaAmount : 0
}

const MY_REDEEMER = Redeemer::Mint
