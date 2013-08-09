#!/bin/sh
git filter-branch -d /run/user/brandon/migration --env-filter '

n=$GIT_AUTHOR_NAME
m=$GIT_AUTHOR_NAME

case ${GIT_AUTHOR_NAME} in
"Chieu Nguyen") ;;
amoebacity22@gmail.com)
    n="Chieu Nguyen"; m="amoebacity22@gmail.com";;
"Andrei Arusoai") ;;
andrei.arusoaie|andrei.arusoaie@gmail.com|Andrei.Arusoaie@gmail.com)
    n="Andrei Arusoai"; m="Andrei.Arusoaie@gmail.com";;
"Andrei Ștefănescu") ;;
andreistef|andreistef@gmail.com)
    n="Andrei Ștefănescu"; m="andreistef@gmail.com";;
"Aurélien Marion") ;;
aurelienhmarion@gmail.com)
    n="Aurélien Marion"; m="aurelienhmarion@gmail.com";;
"Brandon Moore") ;;
brandon.3.moore|brandon.3.moore@gmail.com)
    n="Brandon Moore"; m="brandon.3.moore@gmail.com";;
"Chris Hathhorn") ;;
chathhorn)
    n="Chris Hathhorn"; m="chathhorn@gmail.com";;
"Chucky Ellison") ;;
cmelliso)
    n="Chucky Ellison"; m="cmelliso@gmail.com";;
"Vicol Daniel-Ionut") ;;
daniel.ionut.vicol)
    n="Vicol Daniel-Ionut"; m="daniel.ionut.vicol@gmail.com";;
"David Lazar") ;;
david.lazar|david.lazar@gmail.com)
    n="David Lazar"; m="david.lazar@gmail.com";;
"Denis Bogdanas") ;;
denis.bogdanas@gmail.com)
    n="Denis Bogdanas"; m="denis.bogdanas@gmail.com";;
"Dorel Lucanu") ;;
Dorel.Lucanu|dorel.lucanu@gmail.com)
    n="Dorel Lucanu"; m="Dorel.Lucanu@gmail.com";;
"Dwight Guth") ;;
dwight.guth@gmail.com)
    n="Dwight Guth"; m="dwight.guth@gmail.com";;
"Emilian Necula") ;;
Emiliannec|Emiliannec@gmail.com)
    n="Emilian Necula"; m="Emiliannec@gmail.com";;
"Felipe Tanus") ;;
fotanus)
    n="Felipe Tanus"; m="fotanus@gmail.com";;
"Grigore Roșu") ;;
grigore.rosu|grigore.rosu@gmail.com)
    n="Grigore Roșu"; m="grigore.rosu@gmail.com";;
"Radu Mereuta") ;;
headness13@gmail.com)
    n="Radu Mereuta"; m="headness13@gmail.com";;
"Irina Mariuca Asavoae") ;;
irina.mariuca)
    n="Irina Mariuca Asavoae"; m="irina.mariuca@gmail.com";;
"Kyle Blocher") ;;
kdblocher@gmail.com)
    n="Kyle Blocher"; m="kdblocher@gmail.com";;
"k-list@cs.uiuc.edu") ;;
k-list@cs.uiuc.edu)
    n="k-list@cs.uiuc.edu"; m="k-list@cs.uiuc.edu";;
"Dmitry Dzhus") ;;
lapoussique@gmail.com)
    n="Dmitry Dzhus"; m="lapoussique@gmail.com";;
"Maurice Rabb") ;;
m3rabb|m3rabb@gmail.com)
    n="Maurice Rabb"; m="m3rabb@gmail.com";;
"Michael Ilseman") ;;
michael.ilseman|michael.ilseman@gmail.com)
    n="Michael Ilseman"; m="michael.ilseman@gmail.com";;
"Mihail Asavoae") ;;
mihail.asavoae|mihail.asavoae@gmail.com)
    n="Mihail Asavoae"; m="mihail.asavoae@gmail.com";;
"Milos Gligoric") ;;
milos.gligoric)
    n="Milos Gligoric"; m="milos.gligoric@gmail.com";;
"Owolabi Legunsen") ;;
owolabileg@gmail.com)
    n="Owolabi Legunsen"; m="owolabileg@gmail.com";;
"Patrick Meredith") ;;
pmeredit)
    n="Patrick Meredith"; m="pmeredit@gmail.com";;
"Ștefan Ciobâcă") ;;
stefan.ciobaca|stefan.ciobaca@gmail.com)
    n="Ștefan Ciobâcă"; m="stefan.ciobaca@gmail.com";;
"Traian Șerbănuță") ;;
traian.serbanuta|Traian.Serbanuta|traian.serbanuta@gmail.com|Traian.Serbanuta@gmail.com)
    n="Traian Șerbănuță"; m="Traian.Serbanuta@gmail.com";;
"Vlad Rusu") ;;
Vlad.Rusu@inria.fr)
    n="Vlad Rusu"; m="VladMGRusu@gmail.com";;
"Cansu Erdogan") ;;
cnserd|cnserd@gmail.com)
    n="Cansu Erdogan"; m="csnerd@gmail.com";;
"Elena Naum") ;;
naum.elena|naum.elena@gmail.com)
    n="Elena Naum"; m="naum.elena@gmail.com";;
"(no author)")
    n="(no author)"; m="nobody@domain.invalid";;
*)
    exit 1;;
esac

export GIT_AUTHOR_NAME="$n"
export GIT_AUTHOR_EMAIL="$m"
export GIT_COMMITTER_NAME="$n"
export GIT_COMMITTER_EMAIL="$m"
' -- --all
