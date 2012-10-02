#
# Groupy By, raw implementation
#
groupbySingle = function(byVect, xDF, fun) {
    stopifnot(length(byVect) == dim(xDF)[1])
    if (! is.factor(byVect))
        byVect = as.factor(byVect)
    byVectEncode = unclass(byVect)
    agg = groupbyId(byVectEncode, xDF, fun) 
    cbind(attr(byVectEncode, 'levels'), agg)
}
groupbyId = function(byVectEncode, xDF, fun) {
    agg = sapply(1:max(byVectEncode), function(id) {
        isId = byVectEncode == id
        row = sapply(xDF[isId, ], fun)
    })
    agg = t(agg)
}


groupbyMultiple = function(byDF, xDF, fun) {
    stopifnot(dim(byDF)[1] == dim(xDF)[1])
    #TODO
}
groupbyIds= function(byDF, xDF, fun) {
    # seems recursive
}
groupbyIdVer2 = function(byVectEncode, xDF, fun) {
    agg = sapply(xDF, function(x) {
        sapply(1:max(byVectEncode), function(id) {
            isId = byVectEncode == id
            fun(x[isId])
        })
    })
}

    
ut_groupby = function() {
    actor = c('act2', 'act1', 'act2', 'act3', 'act1', 'act1')
    x = data.frame(salary=c(200, 100, 20, 300, 400, 10), 
        timeCost=c(20, 10, 200, 30, 40, 100))
    groupbyImplRaw(actor, x, max)
}
    
#
# Unique, returning indices
#    
utilUnique = function(xMat, dimn=1) {
    uMat = unique(xMat, MARGIN=dimn)
    ind = rep(-1, dim(xMat)[dimn])
    funMatch = function(uVect, xMat) {
            apply(xMat, dimn, function(v) { 
                tf = v == uVect
                all(tf)
            })
        }
        
    id = 1 : dim(uMat)[dimn]
    for (i in id) {
        e = utilSubMat(i, uMat, dimn)
        isMatch = funMatch(e, xMat)
        browser()
        ind[isMatch] = i
    }
    return(list(val=uMat, ind=ind))
}

ut_utilUnique = function() {
    xMat = matrix(c(1, 2, 2, 3, 1, 2, 2, 4, 2, 3), ncol=2, byrow=T)
    print(xMat)
    rst = utilUnique(xMat)
    #print(rst['val'])
    #print(rst['ind'])
}

#
# For high-D array, use abind::asub(), from library(abind)
#
utilSubMat = function(i, x, dimn=1) {
    #rst = if (dimn ==1) x[i, ] else x[, i]
    if (dimn == 1) {
        rst = x[i, ]
    } else {
        rst = x[, i]
    }
}

utilAssertEqualMat = function(m1, m2) {
    stopifnot(all(dim(m1) == dim(m2)))
    mapply(function(x1, x2) stopifnot(x1==x2), m1, m2)
}
    
     
