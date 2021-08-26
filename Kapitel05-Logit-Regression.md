5. Logistische Regression
================
Hendrik Jürges

 <script src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" type="text/javascript"></script>
------------------------------------------------------------------------

### Lernziele und Literatur

Die logistische Regression wird für die multivariate Beschreibung der
bedingten Wahrscheinlichkeit von Ereignissen verwendet. Die Studierenden
können die geschätzten Koeffizienten der logistischen Regression korrekt
interpretieren. Sie können alternative Schätzmethoden anwenden und
verstehen, wann das lineare Regressionsmodell auch bei binären
abhängigen Variablen angemessen ist.

##### Literatur

-   Gelman & Hill, *Data Analysis Using Regression and
    Multilevel/Hierarchical Models*, Kapitel 5.1, 5.2, 5.4

------------------------------------------------------------------------

### Einführung

Logistische Regression (auch Logit-Regression genannt) ist eine
Standardprozedur zur Berechnung von Modellen mit einer binären
*abhängigen* Variable. In vielen Anwendungen der empirischen
Gesundheitsökonomik machen wir Vorhersagen von Merkmalen mit nur zwei
Ausprägungen:

-   Gesundheitszustände: ob jemand Diabetes hat, ob jemand fettleibig
    ist, ob jemand bei schlechter Gesundheit ist, ob jemand körperlich
    behindert ist, ob jemand tot ist
-   Gesundheitsverhalten: ob jemand raucht, Alkohol trinkt, zweimal in
    der Woche joggt, fettes Essen isst, sich hat impfen lassen
-   Versicherungsstatus: ob jemand überhaupt versichert ist, ob jemand
    privat oder gesetzlich versichert ist, ob jemand eine
    Zusatzversicherung hat
-   Inanspruchnahme: ob jemand zum Arzt gegangen ist, einen
    Krankenhausaufenthalt hatte, ein bestimmtes Medikament einnimmt.

Die Schätzung von logistischen Regressionsmodellen, die derartige
Vorhersagen erlauben, ist mit Hilfe von R einfach. Die Interpretation
der Regressionskoeffizienten bereitet dagegen oft Schwierigkeiten.

Als Beispiel betrachten wir mögliche Prädiktoren des Selbstbilds von
Kindern und Jugendlichen von ihrem Körper. Dazu laden wir erneut den
bekannten HBSC-Datensatz. Im folgenden sind die ersten drei Fälle
nochmals tabelliert.

``` r
load(file = "hbsc_auszug.Rdata")
head(hbsc, 3)
```

    ## # A tibble: 3 x 5
    ##   sex     age bodyheight bodyweight thinkbody           
    ##   <fct> <dbl>      <int>      <int> <fct>               
    ## 1 Girl   13.2        166         57 Much too fat        
    ## 2 Girl   15.5        169         52 About the right size
    ## 3 Boy    11.8        162         40 Much too thin

Wir wollen nun herausfinden, wie die “objektiven” Körpermaße, gemessen
durch das Gewicht, oder besser den Body-Mass-Index, mit dem Selbstbild
der Kinder und Jugendlichen zusammenhängen. Naheliegend ist die
Vermutung, dass der Anteil derjenigen, die sich für “zu dick” halten,
mit dem BMI steigt. Nun hat das Merkmal `thinkbody` fünf verschiedene
Ausprägungen, die wir zu einem binären Faktor-Merkmal `think_fat`
zusammenfassen.[1] Wer sich etwas zu dick oder viel zu dick findet, wird
als “zu dick” klassifiziert. Alle anderen werden als “nicht zu dick”
klassifiziert:

<!-- ```{r} -->
<!-- hbsc$think_fat <- factor( -->
<!--   ifelse(hbsc$thinkbody %in% c("A bit too fat","Much too fat"), 2, 1), -->
<!--   labels = c("Nicht zu dick", "Zu dick") -->
<!--   ) -->
<!-- hbsc %>% flat_table(thinkbody, think_fat) -->
<!-- ``` -->

``` r
# Denken, dass man zu dick ist
hbsc$think_fat <- factor(ifelse(hbsc$thinkbody=="A bit too fat" | 
                                  hbsc$thinkbody=="Much too fat", 2, 1),
                         labels = c("Nicht zu dick", "Zu dick"))
# Kontrolle der Zuordnung durch Kreuztabelle
hbsc %>% flat_table(thinkbody, think_fat)
```

    ##                      think_fat Nicht zu dick Zu dick
    ## thinkbody                                           
    ## Much too thin                              7       0
    ## A bit too thin                            64       0
    ## About the right size                     184       0
    ## A bit too fat                              0     199
    ## Much too fat                               0      34

Den BMI müssen wir erneut als Gewicht durch Größe in Meter zum Quadrat
berechnen. Aus Gründen der Darstellung teilen wir das Merkmal `bmi`
zunächst in Quintile auf, d.h. fünf *gleich große*, aufsteigend nach BMI
sortierte, Gruppen von Kindern (`bmi_quintile`, von 1 = niedriger BMI
bis 5 = hoher BMI). Die Aufteilung des BMI in Quintile erfolgt mit der
Funktion `ntile()`:

``` r
# BMI-Kategorien
hbsc <- hbsc %>% 
  mutate(bmi = bodyweight/(bodyheight/100)^2,
         bmi_quintile = ntile(bmi, 5))
hbsc %>% group_by(bmi_quintile) %>% 
  summarise(n = n(), min = min(bmi), max = max(bmi))
```

    ## # A tibble: 6 x 4
    ##   bmi_quintile     n   min   max
    ##          <int> <int> <dbl> <dbl>
    ## 1            1    90  12.2  16.8
    ## 2            2    90  16.9  18.3
    ## 3            3    90  18.3  19.8
    ## 4            4    89  19.9  22.0
    ## 5            5    89  22.1  32.5
    ## 6           NA    52  NA    NA

Alle Gruppen sind gleich groß (bis auf unvermeidliche Schwankungen, da
die Anzahl an Beobachtungen nicht ohne Rest durch 5 teilbar ist).
Mittels `group_by` und `summarise()` können wir uns über die
Intervallgrenzen der Quintile informieren (Minimum und Maximum). So
reicht z.B. das vierte Quintil von einem BMI von 19.8 bis 22.0. Nachdem
wir nun die beiden Hauptmerkmale unserer Analyse erstellt haben, können
wir den Zusammenhang tabellarisch und graphisch darstellen. Wir beginnen
mit einer einfachen Häufigkeitsauszählung. Die Interpretation der
Tabelle sollte keine Schwierigkeiten bereiten.

``` r
# Einfache Häufigkeitsauszählung
hbsc %>% flat_table(think_fat, bmi_quintile)
```

    ##               bmi_quintile  1  2  3  4  5
    ## think_fat                                
    ## Nicht zu dick              76 66 44 38 14
    ## Zu dick                    12 24 43 51 74

Absolute Häufigkeiten zu vergleichen ist aber oft zur Beantwortung
inhaltlicher Fragen wenig sinnvoll. Daher berechnen wir relative
Häufigkeiten bzw. Anteile, hier die Anteile derjenigen, die sich nicht
zu dick finden und derjenigen, die sich zu dick finden *innerhalb jedes
BMI Quintils*.

``` r
hbsc %>% flat_table(think_fat, bmi_quintile, margin = "col")
```

    ##               bmi_quintile     1     2     3     4     5
    ## think_fat                                               
    ## Nicht zu dick              86.36 73.33 50.57 42.70 15.91
    ## Zu dick                    13.64 26.67 49.43 57.30 84.09

<!-- ```{r} -->
<!-- invlogit <- function(x) { -->
<!--   exp(x)/(1+exp(x)) -->
<!-- } -->
<!-- logit <- function(x) { -->
<!--   log(x/(1-x)) -->
<!-- } -->
<!-- x <- with(hbsc, logit(prop.table(table(think_fat, bmi_quintile),margin = 2))) -->
<!-- gglot() -->
<!-- plot(1:5,x[2,]) -->
<!-- ``` -->

Die Tabelle zeigt, dass sich 13.6% aller Kinder im ersten Quintil (also
das Fünftel mit dem niedrigsten BMI) für zu dick halten. Im fünften
Quintil (also im Fünftel mit dem höchsten BMI) sind es dagegen 84.1%.
Das ist ein gewaltiger Unterschied, aber inhaltlich nicht überraschend.
Eine für kategoriale oder binäre Merkmale etwas ungewöhnliche aber
anschauliche Form der Darstellung ist das folgende Streudiagramm. Jeder
Punkt stellt eine Beobachtung dar. Man beachte, dass das Bild erst durch
`geom_jitter()` statt `geom_point()` durch Vermeidung von Overplotting
interessant wird. Man sieht deutlich, wie mit steigendem `bmi_quintile`
die Anzahl und der Anteil derjenigen, die sich zu dick finden, steigt.

``` r
# Als Scatterplot
ggplot(data = na.omit(hbsc),
       aes(x = bmi_quintile, y = think_fat)) +
  geom_jitter(width = 0.2, height = 0.15)
```

<img src="Kapitel05-Logit-Regression_files/figure-gfm/unnamed-chunk-6-1.png" width="500px" style="display: block; margin: auto;" />

<!-- ```{r} -->
<!-- mosaicplot(~ bmi_quintile + think_fat, data = hbsc, color = TRUE, las = 1) -->
<!-- ``` -->

Eine bei kategorialen Prädiktoren nützliche Form der Darstellung ist ein
Säulendiagramm, bei dem für jedes BMI-Quintil eine Säule in Höhe des
Anteils derjenigen, die sich zu dick finden, abgetragen wird, zum
Vorgehen vgl. Kapitel “Datenbeschreibung”:

``` r
hbsc %>% 
  group_by(bmi_quintile) %>% 
  summarise(pct_fat = mean(think_fat == "Zu dick", na.rm = TRUE)) %>% 
  ggplot(aes(x = bmi_quintile, y = pct_fat)) +
  geom_col()
```

<img src="Kapitel05-Logit-Regression_files/figure-gfm/unnamed-chunk-7-1.png" width="500px" style="display: block; margin: auto;" />

<!-- ```{r} -->
<!-- hbsc %>%  -->
<!--   flat_table(think_fat, bmi_quintile, margin = "col") %>%  -->
<!--   data.frame() %>% filter(think_fat=="Zu dick") %>%  -->
<!--   ggplot(aes(x = bmi_quintile, y = Freq)) + -->
<!--   geom_col() -->
<!-- ``` -->
<!-- :::: {.uebbox} -->
<!-- ::: {.center} -->
<!-- **Übungsaufgaben mit R** -->
<!-- ::: -->
<!-- 1. Laden Sie den Datensatz NHANES aus dem gleichnamigen Paket. -->
<!-- 2. Tabellieren Sie den höchsten Bildungsabschluss (`Education`) gegen Diabetes (`Diabetes`). Berechnen Sie die Anteil der Befragten mit Diabetes in jeder Bildungsgruppe. Interpretieren Sie das Ergebnis. -->
<!-- 3. Tragen Sie die Anteile in einem Säulendiagramm ab. -->
<!-- :::: -->

------------------------------------------------------------------------

### Logistische Regression mit einem Prädiktor

Die graphische oder tabellarische Darstellung im vorangehenden Abschnitt
beschreibt den Zusammenhang von BMI und Körperbild sehr gut. Wofür
benötigt man ein Regressionsverfahren? Erstens wird die Darstellung
schnell unübersichtlich, wenn der Prädiktor sehr viel mehr Ausprägungen
hat und man das auch beibehalten möchte. Zweitens wird die Darstellung
schnell unübersichtlich, wenn man einen zweiten oder dritten,
möglicherweise ebenfalls kontinuierlichen, Prädiktor hinzuziehen möchte.
Drittens sind wir weiterhin an *ceteris paribus* Betrachtungen
interessiert. Diese sind mit Hilfe der logistischen Regressionanalyse
einfach durchführbar.

Durch die Logit-Regression wird nicht die Ausprägung einer Variable
selbst, sondern die **Wahrscheinlichkeit** einer Ausprägung (hier sich
dick fühlen) vorhergesagt. Genauer gesagt wird eine nicht-lineare
Transformation der Wahrscheinlichkeit (die sogenannte Logit-Funktion)
durch eine lineare Funktion der Prädiktoren vorhergesagt. Die
entsprechende Schätzgleichung für unser Anwendungsbeispiel lautet:

$$\\mbox{logit}(\\widehat{\\Pr(\\mbox{think\_fat}=\\mbox{"zu dick"}})) = \\alpha + \\beta \\cdot \\mbox{bmi\_quintile}$$

Links des Gleichheitszeichens steht die Logit-Funktion (logit()) der
vorhergesagten Wahrscheinlichkeit Pr () sich dick zu finden, wobei wir
im folgenden auf das Dach auf der linken Seite als Symbol für einen
geschätzten Wert verzichten wollen. Rechts steht eine lineare Funktion,
wie sie schon von der linearen Regression her bekannt ist. Die
Logit-Funktion hat folgende Form:

$$\\mbox{logit}(x) = \\ln\\left(\\frac{x}{1-x}\\right)$$

Sie überträgt ihr Argument *x* aus einem Wertebereich zwischen 0 und 1
(Wahrscheinlichkeit) in einen Wertebereich zwischen  − ∞ und ∞. Die
Logit-Funktion wird auch als *Link-Funktion* bezeichnet, da sie die
lineare Vorhersage rechts des Gleichheitszeichens
(*α* + *β* ⋅ bmi\_quintile) mit der Wahrscheinlichkeit
Pr (think\_fat = "zu dick") verbindet.

Die *inverse* Logit-Funktion:

$$\\mbox{logit}^{-1}(x)=\\frac{e^x}{1+e^x}$$

überträgt ihrerseits ihr Argument *x* aus einem Wertebereich ( − ∞, ∞)
in das Intervall (0,1). Die Schätzgleichung des Logit-Modells lässt sich
– indem auf beiden Seiten des Gleichheitszeichens die inverse
Logit-Funktion angewandt wird – auch als

Pr (think\_fat = "zu dick") = logit<sup> − 1</sup>(*α* + *β* ⋅ bmi\_quintile)

schreiben. Nun steht links des Gleichheitszeichens die gesuchte
Wahrscheinlichkeit als **nicht-lineare** Funktion des Prädiktors auf der
rechten Seite. Den Verlauf der inversen Logit-Funktion im Intervall
(-5,5) beschreibt folgende Abbildung, Die inverse Logit-Funktion ist
S-förmig, konvergiert für kleine Werte von *x* gegen Null und für große
Werte von *x* gegen Eins. Ihre größte Steigung hat sie bei *x* = 0.

``` r
invlogit <- function(x) {
  exp(x)/(1+exp(x))
}
ggplot(data.frame(x = c(-5,5)), aes(x)) +
  stat_function(fun = invlogit) +
  ylab("Invlogit(x)")
```

<img src="Kapitel05-Logit-Regression_files/figure-gfm/unnamed-chunk-8-1.png" width="500px" style="display: block; margin: auto;" />

Bemerkung zur R-Syntax: wir erstellen hier mittels `function()` eine
eigene Funktion in R. Wir haben diese Funktion `invlogit()` genannt und
sie berechnet die inverse Logit-Funktion wie oben angegeben. Die
Funktion `stat_function()` wiederum zeichnet in `ggplot()` beliebige
Funktionen, hier unsere neu erstellte `invlogit()`.

------------------------------------------------------------------------

#### Schätzung eines Logit-Modells

Die Schätzung eines Logit-Modells erfolgt mit der Funktion `glm()` (für
**Generalized Linear Model**). Folgende Besonderheiten gegenüber `lm()`
sollte man beachten:

-   Neben der Angabe der Schätzgleichung und der Daten muss das
    zusätzliche Argument `family = binomial()` angegeben werden, welches
    mitteilt, dass es sich um eine binäre abhängige Variable handelt.
    Dann wird per Voreinstellung eine Logit-Regression geschätzt.
-   Bei der Spezifikation der abhängigen Variable haben wir einfach
    `think_fat` angegeben. Dabei muss man etwas aufpassen. Oben wurde
    `think_fat` so kodiert, dass das ersten Level “Nicht zu dick” und
    das zweite Level “Zu dick” ist. R schätzt die Wahrscheinlichkeit des
    zweiten Levels. Hätte man oben umgekehrt kodiert, dann würde man die
    Wahrscheinlichkeit schätzen, sich für “nicht zu dick” zu halten. Um
    Missverständnisse zu vermeiden, kann man auch den logischen Ausdruck
    `think_fat == "Zu dick"`, der wahr oder falsch sein kann, als
    abhängige Variable wählen. Schließlich ist auch möglich, numerische
    binäre Variable mit den Werten Null und Eins zu verwenden. Dann wird
    die Wahrscheinlichkeit für den Wert 1 geschätzt. Wir verwenden
    dieser Konvention folgend auch oft den kürzeren Ausdruck
    Pr (think\_fat = 1) für Pr (think\_fat = "zu dick").

``` r
glm(think_fat ~ bmi_quintile, data = hbsc, family = binomial()) %>% 
  print(digits = 2)
```

    ## 
    ## Call:  glm(formula = think_fat ~ bmi_quintile, family = binomial(), 
    ##     data = hbsc)
    ## 
    ## Coefficients:
    ##  (Intercept)  bmi_quintile  
    ##        -2.63          0.81  
    ## 
    ## Degrees of Freedom: 441 Total (i.e. Null);  440 Residual
    ##   (58 Beobachtungen als fehlend gelöscht)
    ## Null Deviance:       610 
    ## Residual Deviance: 500   AIC: 500

Der Output enthält neben den Koeffizienten viele Informationen, die wir
nicht benötigen. Die Schätzgleichung lautet:

Pr (think\_fat = "zu dick") = logit<sup> − 1</sup>( − 2.62 + 0.81 ⋅ bmi\_quintile)

Wir können die Vorhersage dieses Modells für beliebige Werte von
bmi\_quintile als Funktion darstellen, hier für alle Werte zwischen 1
und 5.

<img src="Kapitel05-Logit-Regression_files/figure-gfm/unnamed-chunk-10-1.png" width="500px" style="display: block; margin: auto;" />

Die Krümmung ist hier weniger stark als in der vorherigen Abbildung, da
wir uns eher in der “Mitte” der Funktion bewegen (d.h. in der Nähe einer
Vorhersage von 50%). Dennoch dürfte erkennbar sein, dass der geschätzte
Unterschied in Pr(think\_fat=“zu dick”) bei einer konstanten Änderung
von `bmi_quintile` (z.B. um eine Einheit) nicht konstant ist. Das stellt
einen offensichtlichen Gegensatz zur linearen Regression dar.

*Beispiele:*

Bei `bmi_quintile = 1` beträgt die vorhergesagte Wahrscheinlichkeit

logit<sup> − 1</sup>( − 2.62 + 0.81 ⋅ 1) = logit<sup> − 1</sup>( − 1.81) = 0.14

Bei `bmi_quintile = 2` beträgt die vorhergesagte Wahrscheinlichkeit

logit<sup> − 1</sup>( − 2.62 + 0.81 ⋅ 2) = logit<sup> − 1</sup>( − 1.00) = 0.27

Die geschätzte Wahrscheinlichkeit, sich dick zu fühlen, steigt also
zwischen der ersten und zweiten Gruppe um 13 *Prozentpunkte*. Bei
`bmi_quintile = 4` beträgt die vorhergesagte Wahrscheinlichkeit

logit<sup> − 1</sup>( − 2.62 + 0.81 ⋅ 4) = logit<sup> − 1</sup>(0.62) = 0.65

Bei `bmi_quintile = 5` beträgt die vorhergesagte Wahrscheinlichkeit

logit<sup> − 1</sup>( − 2.62 + 0.81 ⋅ 5) = logit<sup> − 1</sup>(1.43) = 0.81

Die geschätzte Wahrscheinlichkeit, sich dick zu fühlen, steigt zwischen
der vierten und fünften Gruppe um 16 Prozentpunkte. Der Unterschied zu
Steigerung zwischen der ersten und zweiten Gruppe ist hier nicht sehr
groß, da wir uns wie bereits erwähnt in der “Mitte” der Funktion
befinden, und diese dort nahezu linear verläuft.

------------------------------------------------------------------------

#### Interpretation der Logit-Parameter

Der geschätzte Steigungsparameter für `bmi_quintile` von 0.81 ist
praktisch nicht zu interpretieren (“eine Veränderung von `bmi_quintile`
um eine Einheit erhöht die Logit-Wahrscheinlichkeit sich dick zu fühlen
um 0.81”). Das positive Vorzeichen informiert uns über die Richtung des
Zusammenhangs, aber eine quantitative Interpretation ist kaum möglich,
da niemand in Logit-Wahrscheinlichkeiten denken kann. Wir haben oben
gesehen, dass der Zusammenhang von BMI und Wahrscheinlichkeit sich “zu
dick” zu finden von der Ausprägung des BMI abhängt. Es gibt also ähnlich
wie bei einer linearen Regression mit quadrierten Prädiktoren nicht den
“einen” Wert, der diesen Zusammenhang beschreibt. Den Zusammenhang
zwischen Prädiktor und Wahrscheinlichkeit eines binären Outcomes nennt
man auch den **marginalen Effekt** eines Prädiktors. Um die
unterschiedlich großen marginalen Effekte zu einem “repräsentativen”
Wert zusammenzufassen, haben sich drei Methoden etabliert:

-   Berechnung des marginalen Effekts nach einer *Faustregel*
-   Berechnung des marginalen Effekts *am Mittelwert der Prädiktoren*
-   Berechnung der marginalen Effekte für jede Beobachtung und
    anschließende *Mittelwertbildung aller individuellen Effekte*

##### Faustregel

Wir beginnen mit der pragmatischsten Lösung, der Faustregel. Diese
lautet bei der Logit-Regression: “Teilt man den Logit-Koeffizienten
durch 4, dann erhält man näherungsweise den Effekt der Veränderung des
Prädiktors um eine Einheit auf die Wahrscheinlichkeit des Outcomes”.
Erklärung: ihre größte Steigung in Höhe von *β*/4 hat die inverse
Logit-Funktion in der “Mitte” bei *α* + *β**x* = 0.[2] Teilt man den
Koeffizienten eines Prädiktors *β* durch 4, so erhält man die *maximale*
Veränderung der Wahrscheinlichkeit bei einer Veränderung des Prädiktors
um Eins. In der Mitte der Logit-Funktion (bei Wahrscheinlichkeiten in
der Nähe von 0.5) ist die Approximation an die tatsächliche Differenz
sehr gut, je weiter man sich dem Rand der Logit-Funktion nähert (bei
Wahrscheinlichkeiten nahe Null oder Eins), desto schlechter wird sie. In
unserem konkreten Anwendungsfall ergibt sich ein Wert von 0.81/4 ≈ 0.20.
Interpretation: Kinder, die im jeweils nächsthöheren BMI-Quintil sind,
haben höchstens eine um 20 Prozentpunkte größere Wahrscheinlichkeit,
sich zu dick zu finden.

##### Steigung der inversen Logit-Funktion am Mittelwert

Bei der Berechnung des Effekts einer Veränderung des Prädikors am
Mittelwert unterscheiden wir zwischen kontinuierlichen und diskreten
Prädiktoren. Der Mittelwert von bmi\_quintile ist 3. Behandelt man
bmi\_quintile als diskreten Prädiktor, dann berechnet man die
Veränderung der Wahrscheinlichkeit zwischen bmi\_quintile von 2.5 und
3.5 (also um eine Einheit um den Mittelwert herum):

``` r
invlogit(-2.62 + 0.81 * 3.5) - invlogit(-2.62 + 0.81 * 2.5)
```

    ## [1] 0.1980555

Man beachte, dass wir hier die oben definierte Funktion `invlogit()`
verwenden. Interpretation des Ergebnisses: Kinder, die im jeweils
nächsthöheren BMI-Quintil sind, haben eine um 19.8 Prozentpunkte größere
Wahrscheinlichkeit, sich zu dick zu finden.

Bei kontinuierlichen Variablen erfolgt die Ermittlung der Steigung am
Mittelwert über die erste Ableitung der inversen Logit-Funktion:

$$\\frac{d\\,\\mbox{logit}^{-1}(\\alpha+\\beta x)}{d\\,x} = \\frac{\\beta e^{\\alpha+\\beta x}}{(1+e^{\\alpha+\\beta x})^2}$$

am Mittelwert von *x* : *x̄*

$$\\frac{\\beta e^{\\alpha+\\beta \\bar{x}}}{(1+e^{\\alpha+\\beta \\bar{x}})^2} = \\frac{0.81 e^{-2.62 + 0.81 \\cdot 3}}{(1+e^{-2.62+0.81\\cdot 3})^2} = 0.20$$

Interpretation wie oben: Kinder, die im jeweils nächsthöheren
BMI-Quintil sind, haben eine um 20 Prozentpunkte größere
Wahrscheinlichkeit, sich zu dick zu finden.

##### Durchschnittliche marginale Effekte

Die heute am häufigsten angewandte, aber rechenintensivste Methode ist
es, die Veränderung der Wahrscheinlichkeit für alle Beobachtungen im
Datensatz (mit ihren jeweiligen *x*-Werten) zu schätzen und daraus den
Mittelwert zu berechnen. Das R-Paket `margins` bietet mit der
gleichnamigen Funktion eine einfach zu implementierende Möglichkeit:

``` r
library(margins)
glm(think_fat ~ bmi_quintile, data = hbsc, family = binomial()) %>% 
  margins()
```

    ##  bmi_quintile
    ##        0.1537

Interpretation des Outputs: der mittlere Effekt der Veränderung des
Prädiktors um eine Einheit beträgt in unserem Datensatz 15.3
Prozentpunkte. Dieser Wert ist deutlich kleiner als die zuvor
ermittelten Werte, weil in die Berechung auch die Beobachtungen am
linken und rechten Rand der Verteilung von bmi\_quintile eingegangen
sind. Dort sind die marginalen Effekte wie bereits besprochen kleiner.
Damit wir aber auch nochmals deutlich, dass die Faustregel oder die
Berechnung der marginalen Effekte am Mittelwert eine Obergrenze für den
durchschnittlichen marginalen Effekt darstellen. Für den Achsenabschnitt
berechnet `margins()` keine marginalen Effekte, da dies inhaltlich nicht
sinnvoll ist.

<!-- :::: {.uebbox} -->
<!-- ::: {.center} -->
<!-- **Übungsaufgaben mit R** -->
<!-- ::: -->
<!-- 1. Nutzen Sie die NHANES Daten und berechnen Sie ein Logit-Modell der Wahrscheinlichkeit, Diabetes zu haben als Funktion des Bildungsabschlusses. Interpretieren Sie Achsenabschnitt und Steigungskoeffizienten inhaltlich und quantitativ ggfs. unter Verwendung der Faustformel. -->
<!-- 2. Berechnen Sie für das Modell die durchschnittlichen marginalen Effekte. Vergleichen Sie mit den per Faustformel berechneten Ergebnisse. Wie erklären Sie sich die Unterschiede? -->
<!-- :::: -->

------------------------------------------------------------------------

#### Logit-Regression mit BMI als kontinuierlichem Prädiktor

Nun zeigen wir eine logistische Regression, in der wir BMI nicht in
Kategorien eingeteilt haben, sondern als kontinuierlichen Prädiktor
verwenden. Natürlich ändert sich die Interpretation der
Steigungskoeffizienten und marginalen Effekte, da sie nun die
Unterschiede in der geschätzten Wahrscheinlichkeit sich zu dick zu
finden bei einem um einen Punkt höheren BMI angeben.

``` r
glm(think_fat ~ bmi, data = hbsc, family = binomial()) %>% 
  print() %>% 
  margins()
```

    ## 
    ## Call:  glm(formula = think_fat ~ bmi, family = binomial(), data = hbsc)
    ## 
    ## Coefficients:
    ## (Intercept)          bmi  
    ##     -8.1369       0.4101  
    ## 
    ## Degrees of Freedom: 441 Total (i.e. Null);  440 Residual
    ##   (58 Beobachtungen als fehlend gelöscht)
    ## Null Deviance:       610.1 
    ## Residual Deviance: 488   AIC: 492

    ##      bmi
    ##  0.07658

Interpretation:

-   Achsenabschnitt: bei einem BMI von Null… Sinnlos.
-   Koeffizient von `bmi`: Ist der BMI um eine Einheit höher, dann
    erhöht sich die Wahrscheinlichkeit sich zu dick zu finden um etwa 10
    Prozentpunkte (nach Faustregel) bzw. um etwa 8 Prozentpunkte (als
    mittlerer marginaler Effekt).

``` r
glm(think_fat=="Zu dick" ~ bmi, data = hbsc, family = binomial()) %>% 
  fitted() %>% 
  add_column(na.omit(hbsc), p_hat = .) %>% 
  ggplot(aes(x = bmi)) + 
  geom_line(aes(y = p_hat), color = "blue", size = 1) +
  geom_jitter(aes(y = as.numeric(think_fat=="Zu dick")), 
              height = 0.05, 
              alpha = 0.5)
```

<img src="Kapitel05-Logit-Regression_files/figure-gfm/unnamed-chunk-14-1.png" width="500px" style="display: block; margin: auto;" />

Bemerkung zum Verständnis der Syntax (nicht klausurrelevant): In dieser
Pipe schätzen wir zunächst mit `glm()` das Logit-Modell, dann berechnen
wir mit `fitted()` die vorhergesagten Wahrscheinlichkeiten für jeden
Fall, dann fügen wir diese mit `add_column()` unter dem Namen `p_hat`
dem Dataframe `hbsc` hinzu. Dabei müssen wir alle Beobachtungen mit
fehlenden Werten entfernen. Dann zeichnen wir die Kurve der
vorhergesagten Werte mit `geom_line()` und das Streudiagramm mit
`geom_jitter()`. Für die Abbildung mit `ggplot` muss die abhängige
Variable, wenn sie ein Faktor ist, in eine numerische Variable
umgewandelt werden, so dass sie zwischen 0 und 1 liegt.

<!-- ```{r} -->
<!-- # Alternative mit method = "glm" -->
<!-- ggplot(hbsc, aes(x = bmi, y = as.numeric(think_fat)-1)) + -->
<!--   geom_jitter(height = 0.05, alpha = 0.5) + -->
<!--   geom_smooth(method = "glm", method.args = list(family = binomial()), se = F) -->
<!-- ``` -->

------------------------------------------------------------------------

### Logistische Regression mit mehreren Prädiktoren

Das Hinzuziehen weiterer Prädiktoren erfolgt analog zur linearen
Regression. Hier fügen wir das Geschlecht hinzu.

``` r
glm(think_fat ~ bmi + sex, data = hbsc, family = binomial())
```

    ## 
    ## Call:  glm(formula = think_fat ~ bmi + sex, family = binomial(), data = hbsc)
    ## 
    ## Coefficients:
    ## (Intercept)          bmi      sexGirl  
    ##     -9.2309       0.4403       1.0553  
    ## 
    ## Degrees of Freedom: 441 Total (i.e. Null);  439 Residual
    ##   (58 Beobachtungen als fehlend gelöscht)
    ## Null Deviance:       610.1 
    ## Residual Deviance: 466.4     AIC: 472.4

Interpretation: Nach der Faustregel haben Mädchen *bei gleichem BMI*
eine etwa 26 *Prozentpunkte* größere Wahrscheinlichkeit, sich für zu
dick zu halten als Jungen. Der Logit-Koeffizient von `bmi` ist um etwa
0.03 größer geworden. Die Abbildung der Logit-Funktionen für Jungen und
Mädchen zeigt, dass der geschätzte Unterschied zwischen Jungen und
Mädchen (der vertikale Abstand zwischen den Kurven) vom BMI selbst
abhängt. Er ist kleiner an den Rändern und größer in der Mitte der
Verteilung des BMI.

``` r
glm(think_fat ~ bmi + sex, data = hbsc, family = binomial()) %>% 
  fitted() %>% 
  add_column(na.omit(hbsc), p_hat = .) %>% 
  ggplot(aes(x = bmi, color = sex)) + 
  geom_line(aes(y = p_hat), size = 1) +
  geom_jitter(aes(y = as.numeric(think_fat=="Zu dick")), 
              height = 0.05, 
              alpha = 0.5)
```

<img src="Kapitel05-Logit-Regression_files/figure-gfm/unnamed-chunk-16-1.png" width="500px" style="display: block; margin: auto;" />

Beachte: dies gilt auch ohne explizites Einfügen eines
Interaktionseffekts! Dass sich vertikale Abstände der Kurven (also
Unterschiede in den geschätzten Wahrscheinlichkeiten) verändern, ist
alleine der Krümmung der inversen Logit-Funktion geschuldet. Der
vertikale Abstand zwischen den Kurven ist stets in der Mitte am größten.

Das Einfügen von Interaktionstermen ist auch möglich, hierbei wird das
Zentrieren der Variablen *dringend* empfohlen.

``` r
hbsc %>% center(bmi) %>% 
  glm(think_fat ~ bmi_c * sex, data = ., family = binomial()) %>% 
  print(digits = 1)
```

    ## 
    ## Call:  glm(formula = think_fat ~ bmi_c * sex, family = binomial(), data = .)
    ## 
    ## Coefficients:
    ##   (Intercept)          bmi_c        sexGirl  bmi_c:sexGirl  
    ##         -0.63           0.41           1.08           0.08  
    ## 
    ## Degrees of Freedom: 441 Total (i.e. Null);  438 Residual
    ##   (58 Beobachtungen als fehlend gelöscht)
    ## Null Deviance:       600 
    ## Residual Deviance: 500   AIC: 500

Hieraus ergibt sich die Schätzgleichung:

$$\\begin{align\*}
\\Pr(\\mbox{think\_fat} = 1) ={}& \\mbox{logit}^{-1}(-0.63 + 0.41 \\cdot \\mbox{bmi\_c} + 1.08 \\cdot \\mbox{sexGirl}\\\\
& + 0.08 \\cdot \\mbox{bmi\_c} \\cdot \\mbox{sexGirl})
\\end{align\*}$$

Interpretation:

-   Achsenabschnitt: bei mittlerem BMI haben Jungen eine geschätzte
    Wahrscheinlichkeit sich zu dick zu fühlen von invlogit(-0.63)=0.35
-   Koeffizient von `bmi_c`: Bei Jungen erhöht sich die geschätzte
    Wahrscheinlichkeit sich zu dick zu fühlen um höchstens ca. 0.41/4 =
    0.10, also 10 Prozentpunkte, wenn der BMI um einen Punkt steigt
-   Koeffizient von `sexGirl`: Mädchen mit mittlerem BMI haben eine ca.
    1.08/4=0.27, also 27 Prozentpunkte, höhere geschätzte
    Wahrscheinlichkeit sich zu dick zu fühlen als Jungen
-   Koeffizient von `bmi_c:sexGirl`: Bei Mädchen steigt die geschätzte
    Wahrscheinlichkeit sich zu dick zu finden mit jedem BMI-Punkt um ca.
    0.08/4 = 0.02 (also 2 Prozentpunkte) stärker als bei Jungen. Die
    Abbildung zeigt auch, dass die inverse Logit-Funktion für Mädchen
    meist steiler verläuft als für Jungen

<img src="Kapitel05-Logit-Regression_files/figure-gfm/unnamed-chunk-18-1.png" width="500px" style="display: block; margin: auto;" />

------------------------------------------------------------------------

### Odds Ratios (Chancenverhältnisse)

In der medizinschen Literatur herrscht die Interpretation der Ergebnisse
der logistischen Regression über Odds Ratios (Chancenverhältnisse) vor.
Hat ein Ereignis die Wahrscheinlichkeit *p*, dann bezeichnet man
$\\frac{p}{1-p}$ als Chance oder auf Englisch **Odds**. Beispiele:

$$\\begin{align\*}
p=1/2 & \\rightarrow \\mbox{Chance}=1:1\\\\
p=1/3 & \\rightarrow \\mbox{Chance}=1:2\\\\
p=2/3 & \\rightarrow \\mbox{Chance}=2:1
\\end{align\*}$$

Das Verhältnis zweier Chancen wird als Chancenverhältnis oder auf
Englisch **Odds Ratio** bezeichnet. Beispiele:

-   Steigt die Wahrscheinlichkeit für eine Erkrankung (z.B. hoher
    Blutdruck) durch eine Exposition (z.B. Rauchen) von 0.33 auf 0.5,
    dann erhöht sich die Chance von 1:2 auf 1:1. Das Chancenverhältnis
    zwischen Rauchern und Nichtrauchern beträgt 1:1/1:2 = 2
-   Steigt die Wahrscheinlichkeit für eine Erkrankung (z.B. hoher
    Blutdruck) durch eine Exposition (z.B. Rauchen) von 0.5 auf 0.67,
    dann erhöht sich die Chance von 1:1 auf 2:1. Das Chancenverhältnis
    zwischen Rauchern und Nichtrauchern beträgt 2:1/1:1 = 2

Bei kleinen *p* approximieren Chancenverhältnisse relative Risiken, bei
großen *p* fallen Odds Ratios und relative Risiken auseinander.
Beispiele:

-   Steigt die jährliche Wahrscheinlichkeit für eine Erkrankung (z.B.
    Brustkrebs) durch eine Exposition (z.B. Kaffee trinken) von 0.020
    auf 0.025, dann beträgt das relative Risiko einer Kaffeetrinkerin
    0.025/0.020 = 1.25. In Chancen ausgedrückt ergibt sich eine
    Steigerung von 1:49 auf 1:39. Das Chancenverhältnis zwischen
    Kaffeetrinkerinnen und Nicht-Kaffeetrinkerinnen beträgt 1:39/1:49 =
    49/39 = 1.25641. Chancenverhältnis und relatives Risiko liegen hier
    also nahe beieinander.
-   Steigt die Wahrscheinlichkeit für eine Erkrankung (z.B. hoher
    Blutdruck) durch eine Exposition (z.B. Rauchen) von 0.33 auf 0.5,
    dann beträgt das relative Risiko 0.5/0.33 = 1.5. Das
    Chancenverhältnis ist aber wie bereits berechnet 2.

Was ist nun der Zusammenhang zwischen logistischer Regression und Odds
Ratios? Interessanterweise können Logit-Koeffizienten, auf die die
Exponentialfunktion angewandt wurde, als Odds Ratios verbunden mit der
Erhöhung eines Prädiktors um eine Einheit interpretiert werden. Zunächst
stellen wir fest, dass per Definition gilt:
logit(*p*) = ln (*o**d**d**s*). Auf unser empirisches Beispiel bezogen:

$$\\mbox{logit} (\\Pr(\\mbox{think\_fat} = 1)) = \\ln \\frac{\\Pr(\\mbox{think\_fat} = 1)}{1-\\Pr(\\mbox{think\_fat} = 1)}
     = \\ln \\frac{\\Pr(\\mbox{think\_fat} = 1)}{\\Pr(\\mbox{think\_fat} = 0)}$$

D.h. mit einer logistischen Regression schätzen wir auch:

$$\\ln \\frac{\\Pr(\\mbox{think\_fat} = 1)}{\\Pr(\\mbox{think\_fat} = 0)} = \\alpha + \\beta \\cdot \\mbox{bmi\_quintile}$$

Exponentieren auf beiden Seiten der Gleichung ergibt:

$$\\frac{\\Pr(\\mbox{think\_fat} = 1)}{\\Pr(\\mbox{think\_fat} = 0)} = \\exp(\\alpha + \\beta \\cdot \\mbox{bmi\_quintile})=\\exp(\\alpha)\\cdot \\exp(\\beta)^{\\small \\mbox{bmi\_quintile}}$$

Steigt bmi\_quintile um eine Einheit, dann verändern sich die
geschätzten Odds um den Faktor exp (*β*). Wir rechnen das für unser
Beispiel mit R als Taschenrechner durch:

``` r
bmi_quintile <- c(1:5)      # Erzeugt Vektor mit den Zahlen 1 bis 5
chance <- exp(-2.62 + 0.81 * bmi_quintile) # berechnet die Odds für die Elemente des Vektors
tibble(bmi_quintile, chance) # Verbindet Vektoren zu Tibble und gibt diesen aus
```

    ## # A tibble: 5 x 2
    ##   bmi_quintile chance
    ##          <int>  <dbl>
    ## 1            1  0.164
    ## 2            2  0.368
    ## 3            3  0.827
    ## 4            4  1.86 
    ## 5            5  4.18

Die Chance für das Ereignis “Kind fühlt sich zu dick” steigt mit dem BMI
Quintil exponentiell an. Das Chancenverhältnis bei einer Erhöhung der
BMI Kategorie um Eins erhält man, indem man eine Chance durch die Chance
der vorangehenden Kategorie teilt:

``` r
chance[2:5]/chance[1:4]
```

    ## [1] 2.247908 2.247908 2.247908 2.247908

Das Chancenverhältnis ist bei jedem Schritt gleich 2.25, egal ob ich von
1 zu 2 oder von 4 zu 5 wechsle. 2.25 ist wiederum genau gleich
exp(0.81), also dem exponentierten Wert des Logit-Steigungskoeffizienten
selbst. Die Eigenschaft der Odds Ratios, über den gesamten Wertebereich
des Prädiktors konstant zu sein, macht diese u.a. so attraktiv. Weniger
attraktiv ist indes, dass man Odds Ratios im Gegensatz zu relativen
Risiken kaum sinnvoll interpretieren kann. Odds Ratios sind nur dann
brauchbare Kennzahlen für einen Zusammenhang, solange diese nahe bei
Eins sind (bzw. die Logit-Koeffizienten nahe Null sind), da man sie dann
approximativ als relative Risiken interpretieren kann. Im Gegensatz zu
Medizinern ziehen Ökonomen daher die Interpretation von
Logit-Regressionen über absolute Veränderungen von Wahrscheinlichkeiten
vor (marginale Effekte).

------------------------------------------------------------------------

#### Berechung von Odds Ratios nach Logit-Regression

Zur Berechung der Odds Ratios stehen verschiedene Möglichkeiten zur
Verfügung. So kann man etwa den Output von `glm()` selbst ergänzen. Im
folgenden Beispiel berechnen wir die Odds Ratios und lassen diese
zusätzlich ausgeben. Den Achsenabschnitt entfernen wir, da dafür eine
Berechnung der Odds Ratios genauso wenig Sinn ergibt wie für marginale
Effekte.

``` r
glm(think_fat ~ bmi + sex, data = hbsc, family = binomial(link = "logit")) %>%
  broom::tidy() %>% filter(term != "(Intercept)") %>% 
  mutate(OR = exp(estimate))
```

    ## # A tibble: 2 x 6
    ##   term    estimate std.error statistic  p.value    OR
    ##   <chr>      <dbl>     <dbl>     <dbl>    <dbl> <dbl>
    ## 1 bmi        0.440    0.0483      9.11 8.15e-20  1.55
    ## 2 sexGirl    1.06     0.233       4.53 5.97e- 6  2.87

<!-- ```{r} -->
<!-- mod <- glm(think_fat ~ bmi + sex, data = hbsc, family = binomial(link = "logit")) -->
<!-- cbind(beta=coef(mod)[-1],or=exp(coef(mod)[-1])) -->
<!-- ``` -->

Interpretation:

-   OR von `bmi`: Mit dem BMI-Punkte erhöht sich die Wahrscheinlichkeit,
    sich zu dick zu finden ceteris paribus um 55 Prozent. Bei dieser
    Interpretation haben wir den möglicherweise erheblichen
    Approximationsfehler einfach ignoriert.
-   OR von `sexGirl`: Mädchen haben bei gleichem BMI eine 2.87-fache
    Wahrscheinlichkeit, sich zu dick zu fühlen als Jungen. Auch hier
    wurde der Approximationsfehler ignoriert.

Nun möchte man möglicherweise auch wissen, ob die Zusammenhänge
statistisch signifikant sind. Dazu könnte man sich den p-Wert der
Logit-Koeffizienten anschauen. Standardfehler für Odds Ratios
auszurechnen ist zwar auch möglich, aber nicht zu empfehlen, da diese
leicht zu Fehlinterpretationen führen können. Die Nullhypothese “x hat
keinen Zusammenhang mit y” lautet nämlich bezogen auf die Odds Ratios,
dass letztere gleich 1 sind. Da wir aber gewohnt sind, gegen 0 zu
testen, passieren hier oft Fehler. Eine sinnvollere Darstellungsform der
Schätzunsicherheit erhält man über die Konfidenzintervalle. Auch dies
können wir selbst einfach berechnen:

``` r
glm(think_fat ~ bmi + sex, data = hbsc, family = binomial(link = "logit")) %>%
  broom::tidy() %>% filter(term != "(Intercept)") %>% 
  mutate(OR = exp(estimate),
         lower95ci = exp(estimate - 1.96 * std.error),
         upper95ci = exp(estimate + 1.96 * std.error)) %>% 
  select(term, OR, lower95ci, upper95ci)
```

    ## # A tibble: 2 x 4
    ##   term       OR lower95ci upper95ci
    ##   <chr>   <dbl>     <dbl>     <dbl>
    ## 1 bmi      1.55      1.41      1.71
    ## 2 sexGirl  2.87      1.82      4.54

<!-- ```{r} -->
<!-- glm(think_fat ~ bmi + sex, data = hbsc, family = binomial(link = "logit")) %>%  -->
<!--   parameters::parameters(exponentiate = TRUE) -->
<!-- ``` -->
<!-- ```{r} -->
<!-- x <- summary(mod) -->
<!-- cbind(or=exp(x$coefficients[-1,1]), -->
<!--       lower95ci=exp(x$coefficients[-1,1]-1.96*x$coefficients[-1,2]), -->
<!--       lower95ci=exp(x$coefficients[-1,1]+1.96*x$coefficients[-1,2])) -->
<!-- ``` -->

Auch spezielle, auf epidemiologische Untersuchungen ausgerichtete Pakete
von R erlauben eine einfache Berechung der Odds Ratios und
Konfidenzintervalle. Ein solches Paket heißt `epiDisplay` mit der
Funktion `logistic.display()`. OR und Grenzen der Konfidenzintervalle
sind mit den von uns berechneten identisch. Außerdem wird der p-Wert der
Logit-Koeffizienten für den Test gegen Null ausgegeben.

``` r
glm(think_fat ~ bmi + sex, data = hbsc, family = binomial(link = "logit")) %>%
  epiDisplay::logistic.display(simplified = TRUE)
```

    ##  
    ##               OR lower95ci upper95ci     Pr(>|Z|)
    ## bmi     1.553225  1.412851  1.707545 8.152207e-20
    ## sexGirl 2.872954  1.819334  4.536753 5.971734e-06

------------------------------------------------------------------------

### Probit-Regressionen

Unter Ökonomen verbreiteter als Logit-Regressionen sind
Probit-Regressionen. Hierbei wird die inverse Logit-Funktion
logit<sup> − 1</sup>() durch die Verteilungsfunktion der
Standardnormalverteilung *Φ*() ersetzt. Die allgemeine Schätzgleichung
lautet:

*P**r*(*y* = 1) = *Φ*(*α* + *β* ⋅ *x*)

In R kann das Modell durch die Spezifikation des alternativen
`link = "probit"` geschätzt werden:

``` r
glm(think_fat == "Zu dick" ~ bmi, 
    data = hbsc, 
    family = binomial(link = "probit")) %>% 
  print(digits = 2)
```

    ## 
    ## Call:  glm(formula = think_fat == "Zu dick" ~ bmi, family = binomial(link = "probit"), 
    ##     data = hbsc)
    ## 
    ## Coefficients:
    ## (Intercept)          bmi  
    ##       -4.86         0.24  
    ## 
    ## Degrees of Freedom: 441 Total (i.e. Null);  440 Residual
    ##   (58 Beobachtungen als fehlend gelöscht)
    ## Null Deviance:       610 
    ## Residual Deviance: 490   AIC: 490

Als Schätzgleichung ergibt sich:

Pr (think\_fat = 1) = *Φ*( − 4.86 + 0.24 ⋅ bmi)

Bei der Interpretation der Koeffizienten stoßen wir auf die gleichen
Probleme wie bei der Logit-Regression. Der geschätzte Koeffizient für
bmi\_quintile von 0.24 ist praktisch nicht zu interpretieren. Die
marginalen Effekte unterscheiden sich je nach Ausprägung der
Prädiktoren. Um die unterschiedlich großen marginalen Effekte zu einem
“repräsentativen” Wert zusammenzufassen, haben sich die gleichen
Methoden etabliert wie bei Logit:

-   Berechnung des marginalen Effekts nach *Faustregel*
-   Berechnung des marginalen Effekts *am Mittelwert der Prädiktoren*
-   Berechnung der marginalen Effekte für jede Beobachtung und
    anschließende *Mittelwertbildung aller individuellen Effekte*

Die Faustregel lautet: “Teilt man den Probit-Koeffizienten durch 2.5,
dann erhält man näherungsweise den Effekt der Veränderung des Prädiktors
um eine Einheit auf die Wahrscheinlichkeit des Outcome teilen”.
Erklärung: Ihre größte Steigung in Höhe von etwa 0.4 ⋅ *β* hat die
Verteilungsfunktion in der “Mitte” bei *α* + *β**x* = 0. Multipliziert
man die Koeffizienten der Prädiktoren *β* mit 0.4 bzw. teilt man sie
durch 1/0.4, so erhält man die *maximale* Veränderung der
Wahrscheinlichkeit bei einer Veränderung der Prädiktoren um 1. In der
Mitte der Funktion ist die Approximation an die tatsächliche Differenz
sehr gut, je weiter man sich dem Rand der Funktion nähert, desto
schlechter wird sie. In unserem konkreten Anwendungsfall ergibt sich ein
Wert von 0.24/2.5 ≈ 0.1. Interpretation: Kinder, deren BMI um einen
Punkt höher ist, haben eine um 10 Prozentpunkte größere
Wahrscheinlichkeit, sich zu dick zu finden. Dieses Ergebnis ist
offenkundig dem der Logit-Regression sehr nahe.

Berechnung bei einer kontinuierlichen Variablen als Steigung am
Mittelwert über die erste Ableitung der Verteilungsfunktion:

$$\\frac{d\\,\\Phi(\\alpha+\\beta x)}{d\\,x} = \\beta \\phi( \\alpha+\\beta x )$$

am Mittelwert von *x* : *x̄*

*β**ϕ*(*α* + *β**x̄*) = 0.24 ⋅ *ϕ*( − 4.86 + 0.24 ⋅ 19.5) ≈ 0.09

Schließlich die Berechnung durch `margins()`:

``` r
glm(formula = think_fat == "Zu dick" ~ bmi, 
    family = binomial(link = "probit"), 
    data = hbsc) %>% 
  margins()
```

    ##      bmi
    ##  0.07654

Auch hier findet man ein mit Logit fast identisches Ergebnis. Das
bedeutet: es kommt auf die Wahl der Link-Funktion (abgesehen von
seltenen Spezialfällen) nicht an. Während die Koeffizienten sehr
unterschiedlich sind, sind die uns eigentlich interessierenden, weil
sinnvoll interpretierbaren, marginalen Effekte praktisch nicht zu
unterscheiden.

------------------------------------------------------------------------

### Lineares Wahrscheinlichkeitsmodell

Unter Ökonomen ebenfalls verbreitet ist das lineare
Wahrscheinlichkeitsmodell (Linear Probability Model oder LPM). Hier wird
die inverse Logit-Funktion logit<sup> − 1</sup>() durch die **Identische
Abbildung** ℐ(*x*) = *x* ersetzt. Die allgemeine Schätzgleichung lautet:

*P**r*(*y* = 1) = ℐ(*α* + *β* ⋅ *x*) = *α* + *β* ⋅ *x*

In R könnte das Modell durch `glm()` mit Spezifikation des alternativen
`link = "identity"` geschätzt werden schätzen, aber einfacher geht es
mit `lm()`:

``` r
lm(think_fat == "Zu dick" ~ bmi, data = hbsc) %>% print(digits=1)
```

    ## 
    ## Call:
    ## lm(formula = think_fat == "Zu dick" ~ bmi, data = hbsc)
    ## 
    ## Coefficients:
    ## (Intercept)          bmi  
    ##       -1.00         0.07

Daraus ergibt sich die Schätzgleichung:

Pr (think\_fat = 1) =  − 1.00 + 0.07 ⋅ bmi

Interpretation:

-   Achsenabschnitt: Geschätzte Wahrscheinlichkeit bei `bmi = 0`. Bei
    geschickter Transformation von `bmi` könnte der Achsenabschnitt
    sinnvoll als Wahrscheinlichkeit am Zentralwert interpretierbar sein.
-   Koeffizient für bmi: unmittelbar als Prozentpunkt-Unterschied in
    Höhe von 0.07 zu interpretieren. Es ist keine Umrechnung
    erforderlich, da die geschätzten Prozentpunktunterschiede bei
    Veränderung von `bmi` beim LPM überall gleich sind. Ein Problem ist,
    dass die geschätzten Wahrscheinlichkeiten außerhalb des Intervalls
    \[0,1\] liegen können. Dies tritt vor allem dann auf, wenn
    Pr (*y* = 1) nahe Null oder Eins liegt, also bei sehr seltenen oder
    sehr häufigen Ereignissen. Wenn dagegen Pr (*y* = 1) zwischen 0.3
    und 0.7 liegt, ist das LPM praktisch gefahrlos anzuwenden und die
    Ergebnisse sind den durchschnittlichen marginalen Effekten eines
    Logit- oder Probit-Modells sehr ähnlich. Tatsächlich liefert das
    lineare Wahrscheinlichkeitsmodell in unserem Anwendungsfall eine
    bessere Approximation an den durchschnittlichen marginalen Effekt
    als die Faustregel oder die Auswertung am Mittelwert des Prädiktors.

#### Vergleich mit Logit und Probit

Die folgende Abbildung illustriert, dass die geschätzten Logit-,
Probit-, und linearen Funktionen im für uns relevanten Wertebereich von
`bmi` nahe beieinanderliegen. Man sieht aber auch, dass beim LPM die
vorhergesagten Wahrscheinlichkeiten links unter Null und rechts über
Eins liegen können.

<img src="Kapitel05-Logit-Regression_files/figure-gfm/unnamed-chunk-27-1.png" width="500px" style="display: block; margin: auto;" />

------------------------------------------------------------------------

### Wiederholungsfragen

Geben Sie an, ob die Aussage wahr oder falsch ist, und begründen Sie
Ihre Antwort. Beziehen Sie sich dabei auf Inhalte des Kapitels, und
nennen Sie zusätzliche Annahmen, die Sie möglicherweise treffen müssen.

1.  Bei der logistischen Regression darf die erklärende Variable
    aufgrund der Krümmung der (inversen) logistischen Funktion nicht
    binär sein.
2.  Von den Regressionskoeffizienten einer logistischen Regression lässt
    sich ohne weitere Berechnungen praktisch nur das Vorzeichen sinnvoll
    interpretieren.
3.  Ein Nachteil des linearen Wahrscheinlichkeitsmodells ist, dass die
    Schätzwerte für bedingte Mittelwerte nicht immer als
    Wahrscheinlichkeiten interpretierbar sind.
4.  Logit- und Probit-Modelle gehen von sehr unterschiedlichen Annahmen
    aus und die Ergebnisse unterscheiden sich daher stark. Welches
    Verfahren verwendet werden kann, muss durch einen Logit-Test geprüft
    werden.
5.  Das bevorzugte Verfahren zur Zusammenfassung der Ergebnisse einer
    Logit- oder Probit-Regression ist die Berechnung mittlerer
    partieller Effekte für alle Beobachtungen im Datensatz.

[1] Es gibt auch Verfahren für abhängige Merkmale mit mehr als zwei
Ausprägungen. Deren Darstellung geht über die Möglichkeiten eines
einführenden Kurses hinaus.

[2] Vergleiche die Ableitung der inversen Logit-Funktion unten.
