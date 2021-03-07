tapez (une fois pour toutes)
chmod +x calc

cette commande a pour effet de rendre le fichier "calc" executable.
allez d'ailleurs regarder le contenu de calc, ca fait toujours plaisir
(cela dit que calc est un petit script qui appelle l'executable main.native).


pour (re)compiler, lancer
make


On peut executer calc de diverses facons, que vous pouvez tester en
tapant :

./calc test1.txt

./catc -debug test1.txt

./calc -shout test1.txt

./calc -shout -debug test1.txt

Voyez les commentaires dans main.ml pour comprendre comment gerer les
deux options possibles.
