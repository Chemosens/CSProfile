data(cheeses)
mam=profileMAM(profileObject=cheeses)
mam2=profileMAM(profileObject=cheeses, output="MAM2.html",correctOnlyIfSignificant=TRUE,limitOfSignificance=0.05,
                negativeCorrection=TRUE)
mam3=profileMAM(profileObject=cheeses, output="MAM3.html",correctOnlyIfSignificant=FALSE,limitOfSignificance=0.05,
                negativeCorrection=FALSE)

# Equality for descriptor 1 between mam and mam2

test_that("equality for descriptor 1 between mam and mam2",
          expect_true(
            all(mam[[1]][1:4,]==mam2[[1]][1:4,])
            )
)

test_that("Non-equality for descriptor 12 between mam and mam2",
          expect_true(
            any(mam[[12]][1:4,]!=mam2[[12]][1:4,])
          )
)

test_that("SS Scaling null for descriptor 12 in mam2",
          expect_true(
            mam2[[12]]["Scaling","SS"]==0
          )
)


test_that("Differences between mam and mam3 (no negative correction)",
          expect_true(
            mam[[1]]["Scaling","SS"]<mam3[[1]]["Scaling","SS"]
          )
)

test_that("Stability in SS disag + SS scaling in  mam and mam3 (no negative correction)",
          expect_true(
            mam[[1]]["Scaling","SS"]+mam[[1]]["Disag","SS"]==mam3[[1]]["Scaling","SS"]+mam3[[1]]["Disag","SS"]
          )
)

# Si on ne fait pas la correction pour les négatifs

