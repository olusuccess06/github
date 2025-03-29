hms_cmatrix <- function(hms_im=matrix()) {
    hms_ci <- NULL
    hms_setM <- function(hms_nm) {
        hms_im <<- hms_nm
        hms_ci <<- NULL
    }
    hms_getM <- function() hms_im
    hms_setI <- function(hms_inv) hms_ci <<- hms_inv
    hms_getI <- function() hms_ci
    list(hms_setM=hms_setM, hms_getM=hms_getM, hms_setI=hms_setI, hms_getI=hms_getI)
}

hms_csolve <- function(hms_mo, ...) {
    hms_ci <- hms_mo$hms_getI()
    if (!is.null(hms_ci)) {
        message("Returning cached inverse")
        return(hms_ci)
    }
    hms_cm <- hms_mo$hms_getM()
    hms_cinv <- solve(hms_cm, ...)
    hms_mo$hms_setI(hms_cinv)
    hms_cinv
}

hms_em <- matrix(c(2,1,1,4),2,2)
hms_sm <- hms_cmatrix(hms_em)
hms_csolve(hms_sm)
hms_csolve(hms_sm)
