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
# Match 2D, returning indices
#    
matchMat = function(xMat, uMat, dimn=1) {
    ind = rep(-1, dim(xMat)[dimn])
    id = 1 : dim(uMat)[dimn]
    for (i in id) {
        e = utilSubMat(i, uMat, dimn)
        isMatch = matchVect(e, xMat, dimn)
        ind[isMatch] = i
    }
    return(ind)
}

#
# wrong! Note merge/join does not keep the order of xMat
#
matchMatVer2 = function(xMat, uMat) {
    stopifnot(dim(xMat)[2] == dim(uMat)[2])
    id = 1 : dim(uMat)[1]
    uMatId = cbind(uMat, id)
    ind = merge(xMat, uMatId, by=1:dim(uMat)[2])$id
    browser()
}
matchVect = function(v, xMat, dimn) {
    apply(xMat, dimn, function(e) { 
        tf = e == v 
        all(tf)
    })
}
     
ut_matchMat = function() {
    dimn = 1
    uMat = matrix(c(1, 2, 2, 3, 3, 4, 4, 5), ncol=2, byrow=T)
    ind = sample(dim(uMat)[1], 10, replace=T)
    print(ind)
    xMat = uMat[ind, ]
    rst = matchMat(xMat, uMat, dimn)
    print(rst)
    stopifnot(all(ind == rst))

    xMat2 = rbind(c(999, 999), xMat, c(888, 888))
    rst2 = matchMat(xMat2, uMat, dimn)
    print(rst2)
    stopifnot(all(c(-1, ind, -1) == rst2))
    print('pass!')
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
    
     
