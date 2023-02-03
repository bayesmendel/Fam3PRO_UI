# validate surgery ages
validateSurgAge <- function(surg.age, cur.age, can.age){
  if(!is.na(surg.age)){
    isNum <- is.numeric(surg.age)
    if(isNum){
      inRange <- (surg.age >= min.age & surg.age <= max.age)
      isInt <- surg.age %% 1 == 0
      if(isInt & inRange){
        if(is.na(can.age) & !is.na(cur.age)){
          noCurAgeConflict <- surg.age <= cur.age
          need(noCurAgeConflict, paste0("Ages must be at or below the person's current age of ", cur.age,"."))
        } else if(!is.na(can.age) & is.na(cur.age)){
          noCanAgeConflict <- surg.age < can.age
          need(noCanAgeConflict, paste0("Prophylactic surgery age must be less than the related cancer age, ", can.age,"."))
        } else if(!is.na(can.age) & !is.na(cur.age)){
          noConflict <- (surg.age < can.age & surg.age <= cur.age)
          need(noConflict, paste0("Prophylactic surgery age must be less than the related cancer age, ", can.age, ", and less than or equal to the current age, ",cur.age,"."))
        }
      } else {
        need(all(isInt, inRange), paste0("Ages must be integers from ", min.age," to ", max.age,"."))
      }
    }
  }
}
