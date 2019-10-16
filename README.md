# Benefit_Demo
This is a demo version of project codes repository.

The main goal of this project is to identify some trends of endoscopy services among three local health providers. One big concern is data heterogeneity.

Hospitals report the claims to the insurers in different ways. For example, Penn State Hershey Endoscopy usually reports a major procedure code (CPT) for an endoscopy event, which makes things straight forward. Unfortunately, other providers don't follow that way. A lot of variations existed in how providers recorded their CPT claims. In some providers, procedures related to anesthesiologist, pathologist were reported separately. Although IBM Watson grouped CPTs in an event by some algorithm, but it is not always reasonable. You can see urine pregnancy tests were also included in endoscopy CPTs. That makes cost comparison less meaningful. Also, claims were issued by individual physicians in some providers. So I had to track them down to the facilities they are affiliated with. Unlike provider names reported by patients in Echo, all the provider names recorded are correct, but are equally valueless to create doctor-facility links because the the name patterns varied across organization structures and affiliation relationships between physicians and facilities. Provider name changes due to the insurer change in 2018 makes the situation more complicated. Attached codes are trying to overcome this heterogeneity and link claims to three major local providers.


One outcome plot example is 
![Example](/images/pokemon.jpg)


![Top_providers_imaging](https://user-images.githubusercontent.com/21266752/66928200-04ef4780-efff-11e9-8cf3-abe670089ed3.jpg)
