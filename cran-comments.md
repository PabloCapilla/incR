## RESUBMISSION 2
This is a resubmission. In this version I have corrected one NOTE and argue that the other is a false positive:


NOTE 1
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pablo Capilla-Lasheras <pacapilla@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  Reynols (17:47)
  incR (11:20)

"Reynols" corrected by "Reynolds" in the new version, the correct reference.
incR is the correct spelling of the package's name.

NOTE 2
* checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.

This NOTE only appears when I include the doi for the referenced book in DESCRIPTION.
I included the doi following direct advice from CRAN.


## RESUBMISSION 1
This is a resubmission. In this version I have:

* Removed "in R" from DESCRIPTION title
* Removed "Analysis of incubation data in R." from DESCRIPTION description
* Added clarity in the type of metrics that the package calculates and included a reference
that widely discusses the type of data that the package handles. These changes have been done 
in DESCRIPTION description.



## Test environments
* local Windows 10 install, R 3.4.1
* win-builder (devel)
* ubuntu 12.04.5 (on travis-ci), R 3.4.0

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE in win-builder (devel):

* checking CRAN incoming feasibility ... NOTE
	Maintainer: 'Pablo Capilla-Lasheras <pacapilla@gmail.com>'

	New submission

	Possibly mis-spelled words in DESCRIPTION:
  		incR (11:20)

No spelling mistake in 11:20, correct package name. And this is, indeed, a new release.


## Reverse dependencies

This is a new release, so there are no reverse dependencies.

---

