---
name: brf-sortering
description: Regler för vilken mejlmapp (under Folders/BrfPärlan) ett mejl i Brf Pärlans Proton-brevlåda ska flyttas till, inklusive känsliga ärenden. Använd när du ska föreslå MoveToFolder eller bedöma var ett mejl hör hemma.
---

# Mejlsortering

Mappstrukturen finns i Proton Bridge under `Folders/BrfPärlan/`. Använd fullständig sökväg som `destinationFolder`. Exempel: `Folders/BrfPärlan/Ekonomi`.

## Grundregler

- Sortera hela tråden till samma mapp som tråden redan ligger i. Splitta inte trådar.
- Välj den mest specifika undermappen. Om ingen undermapp passar, använd huvudmappen.
- Flytta inte mejl som kräver åtgärd och inte är hanterade (`seen = false` och ingen `\Answered`) utan att användaren bett om det. Markera som läst bara när ärendet uppenbart är informationsmejl.
- Är du osäker mellan två mappar: föreslå ingen flytt och förklara varför.
- `INBOX` är arbetsyta. `Archive`, `Spam`, `Trash` och `Sent` används inte av assistenten.

## Mappar och innehåll

Antal mejl är uppskattningar från 2026-11.

| Mapp | Innehåll |
|---|---|
| `Andrahandsuthyrning` (~250) | Ansökningar, förlängningar, hyresavtal, signeringsinbjudningar, OBOS-lista över andrahandsupplåtelser |
| `Avtal & Dokument` (~65) | Avtal (förvaltning, städ), överlämnade handlingar, protokoll från tidigare styrelse |
| `Avtal & Dokument/Teknisk Dokumentation` (~20) | Relationsritningar (el, VVS), drift- och skötselinstruktioner |
| `BesiktningTvåÅr` (~150) | Upphandling och dialog kring garantibesiktning (besiktningsman, OBOS, offerter, tidplan) |
| `BoendeFrågor` (~140) | Frågor och synpunkter från boende, nya medlemmar, nycklar, regler |
| `BoendeFrågor/Kerstin` (~55) | Samlad korrespondens med en enskild boende. Använd bara för tråden den redan innehåller, annars `BoendeFrågor` |
| `Ekonomi` (~325) | Fakturor, avier, bank, förvaltaravtal, budget, försäkring |
| `Ekonomi/Rapporter` (~8) | Månadsrapporter och bokslutsrapporter från förvaltaren (ämne "Rapporter <månad>") |
| `Intresseanmälningar` (~47) | Anmälningar till trivselaktiviteter (t.ex. jubileum, knytkalas), via `trivsel@` |
| `IT_frågor` (~110) | E-post, Boappa-administration, nätverk, webbplats, delade dokument |
| `Lokalerna` (~50) | Gemensamma utrymmen i allmänhet |
| `Lokalerna/Bokningar` (~22) | Bokningar och bekräftelser (gästlägenhet, mötesrum) via `bokning@` |
| `Lokalerna/Uthyrningslokalerna` (~95) | Kommersiella lokaler ("Lokal 3"), hyresgäster, bygglov, mäklare |
| `Lokalerna/Verkstad` (~4) | Verkstadsrummet (`verkstad@`, regler, dokument) |
| `Medlemsregistret` (~100) | Medlemsförändringar, överlåtelser, uppgifter om nya ägare |
| `Medlemsregistret/Anmärkningsvärt` (~2) | Känsliga medlemsärenden (skuld, Kronofogden) – se nedan |
| `Myndighetsinfo` (~30) | Kommunbrev, skannad post från OBOS, nyhetsbrev från kommun/Renhållningsverk |
| `Registrations` (~30) | Kontoinbjudningar och registreringar (Virta, Planima, FusionSolar, kommunens prenumerationer) |
| `Reklam` (~10) | Oönskade erbjudanden (städ, konsulter, energi, styrelseverktyg) |
| `Styrelsemöten` (~80) | Dagordningar, protokoll, förslag inför möten, interna styrelsediskussioner |
| `Styrelsemöten/Årsstämmor` (~3) | Årsstämma, valberedningens förslag, kallelse |
| `Tekniker` (~230) | Fastighetsteknik i allmänhet (ventilationslarm, fjärrvärme, solceller) |
| `Tekniker/Felanmälningar` (~120) | Felanmälningar och uppföljning (OBOS/Envise, Hagtorn, GC Gruppen) |
| `Tekniker/Förvaltning` (~55) | Teknisk förvaltning: städ, skötsel, PHM-kundportal, offerter totalförvaltning |
| `Tekniker/Laddning` (~57) | Elbilsladdning (`laddning@`, Virta, priser) |
| `Tekniker/PHM` (~5) | PHM-systemet: signering, överlåtelse, behörigheter |
| `Tekniker/Service rapporter` (~9) | Servicerapporter (t.ex. hiss, OTIS) |
| `Tekniker/Tvåårsbesiktning` (~180) | Tekniska underlag, felrapporter och uppföljning kring 2-årsbesiktningen |
| `Utemiljö` (~100) | Innergård, gemensamma ytor, kommunens planer i området, parkering |
| `Utemiljö/Balkonger&Terraser` (~5) | Balkongskydd, inglasning (t.ex. Lumon), kulör, takterrass |
| `Utemiljö/SBA` (~1) | Systematiskt brandskyddsarbete, beredskapsplan |
| `Utkommunikationer` (~100) | Utskick till boende: Boappa-inlägg, jubileum, webbplats |
| `Utkommunikationer/Mäklare` (~3) | Kontakt med mäklare (medlemskap, avgifter, energideklaration) |

Tolkningar av mappar där innehållet inte är helt entydigt är gjorda utifrån stickprov. Följ hellre mappen som tråden redan ligger i än den här tabellen om de skiljer sig.

## Snabbregler per avsändare/ämne

- Skannat brev från OBOS/kommun ("Bifogar brev"): `Myndighetsinfo`, men flagga för användaren om brevet kräver svar eller beslut.
- Nyhetsbrev från Lunds kommun, Renhållningsverket, Brunnshög: `Myndighetsinfo`.
- Virta/Planima/FusionSolar-inbjudan: `Registrations` (kopiera aldrig länkar).
- Mejl till `laddning@`: `Tekniker/Laddning`.
- Mejl till `bokning@`: `Lokalerna/Bokningar`.
- Mejl till `trivsel@`: `Intresseanmälningar`.
- Mejl till `verkstad@`: `Lokalerna/Verkstad`.
- Offert- eller säljmejl som styrelsen inte efterfrågat: `Reklam`.
- Garantifel från boende och entreprenör: `Tekniker/Felanmälningar`.
- Andrahandsansökan och signeringsinbjudan (Assently/PHM): `Andrahandsuthyrning`.

## Känsliga ärenden

Flytta till `Medlemsregistret/Anmärkningsvärt` (eller låt ligga kvar) och sammanfatta endast för användaren om mejlet gäller:

- Skulder, betalningsanmärkningar, Kronofogden eller andra personliga ekonomiska förhållanden.
- Hälsa, familjesituation eller liknande personlig information.
- Konflikter mellan grannar eller anmälningar mot en medlem.

Dessa ärenden besvaras inte av assistenten. Styrelsen agerar normalt inte i frågor mellan andrahandsuthyrare och dennes hyresgäst. Ägaren ansvarar mot föreningen.

## Exempel på `reason`

- "Rapporter februari från OBOS Brf-administration. Regel: månadsrapporter ska till Ekonomi/Rapporter."
- "Boende anmäler till laddning via laddning@. Regel: Tekniker/Laddning."
