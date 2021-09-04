Pasaulinio mirtingumo vizualizacija
================

## Tikslai

Šiuo tiriamuoju projektu siekiama ištirti aktualias su žmonių mirtingumu
susijusias charakteristikas ir joms pateikti pritaikyti duomenų
vizualizavimo metodus.

Tyrimo tikslas padalintas į tris smulkesnius uždavinius.

Surasti ir vizualizuoti:

-   Pagrindines mirčių priežasčių, mirčių kiekio pokyčio tendencijas per
    paskutinius kelis dešimtmečius.

-   Šalis (regionus) išsiskiriančias tam tikrais specifiškais mirtingumo
    rodikliais.

-   Įvairių regionų bendresnius mirtingumo profilius, juos palyginti
    tarpusavyje.

## Duomenys

Analizei atlikti naudoti 2 duomenų šaltiniai:

1.  [Our World in Data](https://ourworldindata.com)

<!-- -->

    Istoriniai mirties priežasčių duomenys:

    • šalis ar regionas
    • metai
    • 32 procentinės dalies mirčių dėl tam tikros priežasties kintamieji 
    (širdies ligos, vėžiai, diabetas ir t.t.)

2.  [United Nations Department of Economic and Social
    Affairs](https://population.un.org/wpp/Download/Standard/CSV/)

<!-- -->

    Bendresni mirtingumo/gimstamumo duomenys (istoriniai ir ateities projekcija):

    • šalis ar regionas
    • laikmetis
    • vyrų/moterų/bendra gyvenimo trukmė
    • mirčių/gimimų skaičius 1000 gyventojų
    • naujagimių/iki 5 metų mirtys 1000 gimimų
    • kiti kintamieji

## Naudoti R paketai:

-   highcharter
-   tidyverse
-   hrbrthemes
-   countrycode
-   here
-   ggrepel
-   Xaringan

## Rezultatai

Analizės rezultatus galima rasti [čia](docs/slides.html) (rezultatai
pateikti skaidrių formatu).
