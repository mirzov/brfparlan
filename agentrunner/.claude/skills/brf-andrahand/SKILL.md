---
name: brf-andrahand
description: Hantering av ansökningar om andrahandsuthyrning i Brf Pärlan (förlängning, hyresgäster, jäv, signering, avgift 2027, svar till sökande). Använd för mejl i mappen Andrahandsuthyrning eller när någon vill hyra ut sin lägenhet.
---

# Andrahandsuthyrning

Andrahandsuthyrning kräver styrelsens samtycke (7 kap. 10 § bostadsrättslagen). Beslutet är alltid ett **styrelsebeslut** – assistenten förbereder bara.

## Flöde

1. Ansökan kommer till `styrelsen@` (eller via förvaltaren) med blankett/underlag, ibland som bilaga.
2. Ordföranden eller IT-ansvarig lägger ansökan i mappen `Andrahandsuthyrning` och på dagordningen. Dokument laddas upp i Boappa-mappen "Andrahandsupplåtelser" (endast Boappa-administratör kan ladda upp).
3. Styrelsen beslutar på möte eller via mejlbeslut. Sökande som själv är ledamot deltar inte i handläggning eller beslut (jäv).
4. Vid godkännande signerar **två firmatecknare** (normalt ordförande och viceordförande). Under 2026 sker signering i förvaltarens system (PHM/Assently). Tidigare användes Ebox. Tre signaturer ska inte behövas.
5. Besked skickas till sökande. Bekräftelsen skickas oftast av viceordföranden eller ordföranden.
6. Registrera uthyrningsperiod i förvaltarens system så att avgift och uppföljning blir rätt.

## Regler och praxis i föreningen

- Uthyrningsperioder har varit **12 månader** per ansökan. Förlängning kräver ny ansökan.
- Skäl ska vara beaktansvärda enligt lagen (t.ex. tillfälligt arbete eller studier på annan ort, prova samboskap, sjukdom, familjesituation). Styrelsen gör en individuell bedömning.
- Vid upprepade förlängningar förtydligar styrelsen att fortsatt uthyrning inte kan förutsättas. Exempelmening: "Detta godkännande innebär inte någon garanti för ytterligare förlängning. Vid en ny ansökan görs en förnyad bedömning."
- Ägaren ansvarar för sin hyresgäst och dennes beteende (ordningsregler, skräp vid sopstationen, nycklar och taggar).
- Hyresgäster, släktingar och vänner kan registreras i Boappa av IT-ansvarig. Ägaren ska förse hyresgästen med föreningens regler.
- Styrelsen blir inte mellanhand i tvister mellan ägare och hyresgäst och agerar inte i skuldfrågor (se känsliga ärenden i `brf-sortering`).
- Från **2027-01-01** vill styrelsen att förvaltaren ska debitera avgift för **nya** andrahandsuthyrningar motsvarande 10 % av gällande prisbasbelopp per år (högsta tillåtna nivå) till bostadsrättshavaren. `[verifiera: aktivering hos Bredablick och debiteringsperiod bekräftas ännu inte]`. Ange inte belopp i kronor.
- Bakgrund: i maj 2025 fanns ca 20 andrahandsupplåtelser som överlämnats från OBOS. Dessa sammanställdes i Excel.

## Svarsutkast

- Kvittens: se `brf-svarsstil/references/mallar.md` ("Kvittens utan beslut").
- Beslut: använd mallen "Beslut om andrahandsuthyrning" endast när användaren bekräftat beslutet.
- Saknas uppgifter (period, skäl, hyresgästens namn, undertecknad blankett): lista vad som saknas, utan att bedöma ansökan.

## Typiska fel att undvika

- Skriva att något är "godkänt" utan styrelsebeslut.
- Be om personnummer i mejl. Hänvisa till blanketten eller förvaltarens signeringsflöde.
- Dela uppgifter om hyresgäst med andra boende.
- Kopiera länkar från signeringsinbjudningar.

## Mappning

- Ansökningar, förlängningar, signeringsinbjudan: `Folders/BrfPärlan/Andrahandsuthyrning`.
- Frågor om hur hyresgäster får Boappa-tillgång: `Folders/BrfPärlan/IT_frågor` (tråden "Andrahandsuthyrning och Boappa").
