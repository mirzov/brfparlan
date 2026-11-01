---
name: brf-parlan
description: Startpunkt och index för all e-posthantering i Brf Pärlan i Lund. Använd först vid varje mejlärende (sortering, analys, åtgärdspunkter eller svarsutkast på svenska) och välj därefter rätt ämnes-skill.
---

# Brf Pärlan – index

Läs bara de skills som matchar mejlets ämne. Det håller kontexten liten.

## Steg för varje mejl

1. Identifiera avsändare, ämne och trådhistorik (använd `imap_find_thread_messages` vid behov).
2. Välj ämne enligt tabellen nedan och läs den skillen.
3. Bestäm: (a) flytta till mapp, (b) utkast till svar, (c) bara sammanfatta för användaren, eller (d) lämna orört.
4. Föreslå via `propose_action`. Skicka aldrig.

## Ämnesvägledning

| Mejlet handlar om | Läs skill |
|---|---|
| Vilken mapp mejlet ska ligga i | `brf-sortering` |
| Hur svaret ska formuleras, signatur | `brf-svarsstil` |
| Ansökan/förlängning av andrahandsuthyrning, hyresgäst, hyresavtal | `brf-andrahand` |
| Lägenhetsköp, överlåtelse, medlemsansökan, mäklare, nya ägare, namnbyte i medlemsregister | `brf-medlemmar` |
| Fel i lägenhet/fastighet, garanti, OBOS/Envise, Hagtorn, hiss, sprickor, städ, 2-årsbesiktning | `brf-felanmalan` |
| Laddstolpe, Virta, anmälan till laddning, pris | `brf-laddning` |
| Boka gästlägenhet eller mötesrum, verkstad, kommersiell lokal | `brf-lokaler` |
| Förvaltare, avgift, faktura, månadsrapport, bank, revisor, försäkring, budget | `brf-ekonomi` |
| Dagordning, protokoll, stämma, valberedning, firmateckning | `brf-styrelsemoten` |
| Boappa, e-postlistor, webbplats, domän, delade filer, system-inbjudningar | `brf-it-boappa` |
| Enkel fråga från boende (försäkring, husdjur, nycklar, ritningar m.m.) | `brf-faq-boende` |
| Innergård, växter, balkongskydd, inglasning, parkering, brandskydd, SBA | `brf-utemiljo` |
| Kommun, myndighet, nyhetsbrev, kalenderinbjudan, leverantörsreklam | `brf-externa` |

## Gemensamma principer

- Styrelsen beslutar. Assistenten förbereder.
- Boende ska i första hand hänvisas till rätt instans (entreprenör, Hagtorn, OBOS, Bredablick) i stället för att styrelsen blir mellanhand.
- Allt som rör personliga ekonomiska eller hälsorelaterade förhållanden är känsligt (se `brf-sortering`, avsnitt Känsliga ärenden).
- Aktuella roller, system och kontakter: `brf-context/foreningen.md` och `brf-context/kontakter.md`.

## Vad e-posten visar om arbetsbelastningen

Mängden e-post är stor (över 1 800 i inkorgen, ca 460 skickade). Störst volym: ekonomi, tekniska ärenden (felanmälan, 2-årsbesiktning), andrahandsuthyrning och boendefrågor. Prioritera därför standardiserade svar där FAQ finns, och lämna ovanliga fall till människa.
