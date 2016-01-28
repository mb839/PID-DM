func nhsNumberCheckBool(nhsNumber: String) -> Bool {
	// returns a bool corresponding to "is this a valid NHS number?" 
	// See http://www.datadictionary.nhs.uk/version2/data_dictionary/data_field_notes/n/nhs_number_de.asp?shownav=0	
	// checks that nhsNumber actually contains something
	if nhsNumber.isEmpty {
		return false
	}
    // sets up nhsNum as a separate variable
    var nhsNum:String = nhsNumber
    // removes any dashes or spaces
    nhsNum = String(nhsNum.characters.filter {$0 != " " && $0 != "-"})
    // checks that there are exactly 10 characters
    if nhsNum.characters.count != 10 {
        return false
    }
    // need to declare this as it is an optional? Checks it only contains numbers.
	let nhsInt:Int? = Int(nhsNum)
	if nhsInt == nil {
	    return false
	}
    // removes the last digit
    let rangeCheck = Int(nhsInt! / 10)
	// check that NHS number is in range, see http://systems.hscic.gov.uk/nhsnumber/staff/comms/4788factsheet.pdf
    if (rangeCheck < 400000000 || rangeCheck >= 800000000
	    || (rangeCheck >= 500000000 && rangeCheck < 600000000)) {
	    return false
	}
    // calculates the final check sum before we recycle the rangeCheck variable
    let providedCs:Int = nhsInt! - 10*rangeCheck

    var tot:Int = 0
    var rangeCheckIterate:Int = rangeCheck
    var j:Int = 1
    var multiplier:Int = 10
    // having initialised  tot as 0, then working back from the last digit
	// before the check sum, multiplies by 10 and adds to the total, multiplies 
	// by 9 and adds to the total and so on for all digits.
    while j < 10 {
        tot += multiplier * (rangeCheckIterate - 10*(Int(rangeCheckIterate/10)))
        rangeCheckIterate = Int(rangeCheckIterate/10)
        j++
        multiplier--
    }
    // computes tot(mod 11) then takes this from 11
    var m:Int = 11 - (tot % 11)
    // if m is 10, this NHS number is not valid. If m is 11, sets m to 10
    if m == 10 {
        return false
    } else if m == 11 {
        m = 0
    }
    // returns whether or not m matches providedCs
    return(m == providedCs)
}

var z: Int = 0
var nhs: Int = 4160000000
var nhsChar: String
while z < 64100000 {
    nhsChar = String(nhs)
    nhsNumberCheckBool(nhsChar)
	z++
}

//nhsNumberCheckBool("416 401 3622")
