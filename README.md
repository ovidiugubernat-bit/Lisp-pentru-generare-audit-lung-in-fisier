# LISP-uri de audit pentru desene AutoCAD

## AuditBlocuri.lsp — comanda `AUDITBLOCURI`
Auditul scurt, existent: tip, layer, grup, punct de insertie, text, nume de bloc,
scari si rotatie pentru INSERT, proprietati dinamice.

## AuditPlan.lsp — comenzile `AUDITPLAN` si `AUDITSUS`
Audit profund, gandit ca sursa de date pentru generatoarele automate
(cotare plan de fundatii, notatii stalpi etc.).

In plus fata de auditul scurt:

- **geometrie completa**: capetele liniilor, toate varfurile poliliniilor cu bulge,
  centre/raze/unghiuri, parametrii elipselor;
- **cote**: toate punctele de definitie (10, 11, 12, 13, 14, 15, 16), tipul,
  unghiul, stilul si valorile stilului (DIMSCALE, DIMLFAC, DIMTXT, DIMEXO...),
  masuratoarea si distanta reala intre punctele de definitie, plus suprascrierile
  din XDATA;
- **texte**: inaltime, rotatie, stil, aliniere, punct de aliniere;
- **hasuri**: pattern, scara, unghi, arie, puncte-samanta;
- **grupuri**: numele real din dictionarul ACAD_GROUP (ex. `*A12`), nu adresa
  volatila a entitatii, plus HANDLE-ul fiecarei entitati;
- **antet**: variabile de sistem, tabelele de layere, stiluri de cotare si de text;
- **recapitulare** pe TIP @ LAYER si gabaritul selectiei;
- optional, **toate perechile DXF brute** (raspunde `Da` la intrebarea despre RAW).

### Detectorul de suspecti
La final, raportul listeaza elementele care nu sunt "curate":

| Categorie | Ce inseamna |
|---|---|
| `NEORTOGONAL` | segment care nu e perfect orizontal/vertical; se da abaterea in grade si in mm |
| `LUNGIME NEROTUNDA` | lungime care nu e multiplu al pasului cerut (implicit 2.5 mm) |
| `COTA OBLICA` | punctele de definitie ale cotei nu sunt aliniate ortogonal |
| `COTA NEROTUNDA` | distanta reala intre punctele de definitie nu e rotunda |
| `NOD NEUNIT` | doua capete de segmente la mai putin de 2 mm unul de altul, dar care nu coincid |

Dupa scrierea fisierului se poate parcurge lista cu zoom pe fiecare element
(`[Enter]` = urmatorul, `P` = precedentul, `X` = iesire); la sfarsit toti
suspectii raman selectati, cu grips, ca sa fie usor de gasit.
Inspectia se poate relua oricand cu `AUDITSUS`.

Pragurile se pot schimba din fisier: `*ap:grid*` (pasul de rotunjime, mm),
`*ap:gtol*` (toleranta fata de pas), `*ap:atol*` (toleranta unghiulara),
`*ap:jtol*` (distanta sub care doua capete ar trebui sa coincida).
