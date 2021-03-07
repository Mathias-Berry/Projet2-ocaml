On a fait ensemble la partie débutante moins les options.

Ensuite, Emile a fait la partie lexer et la partie parser, plus l'ajout des options et du fait de devoir donner en entrée un fichier sans les chevrons. J'ai aussi conformé l'affichage d'un code à ce que Caml peut accpeter.
Mathias quant à lui est très moche.


Pour la partie lexer, j'ai juste rajouté ce qu'il manquait, rien de spécial à ajouter.
Pour le parser, j'ai découpé les expressions en deux types, les atomiques ( variables, constantes et machin entre parenthèses ), et le reste ( sachant que dans le fond tout est expression, y compris les booléens ). Ainsi, l'application d'une fonction se detecte par la règle expression atomique, puisque c'est le seul cas ou on peut se retrouver dans cette situation, avec le cas de la règle contenant Uminus. D'où l'ajout de token imaginaire pour ordonner leur priorité. Pour les let in, les fonction et prInt, il faut que l'expression qu'il y a après le let in ou la flèche de la fonction soit la plus grande possible, et donc j'ai donner la priorité minimale à ces règles.
Ensuite, je donne la priorité maximale à la règle aqui renvoie vers atomique car dès qu'on tombe sur une expression qui pourrait être une expression atomique, hop on la reduit.
Ensuite, je ne sais pas pourquoi, il a fallu donner des priorités aux règles de atomique:. De ce que j'avais lu, j'avais déduit qu'il y en avait pas besoin, mais quand je les mets pas il y a des conflits, donc je les mets.
Enfin, pour les fonction définies comme suit : let f x y z = ... alors je récupère la liste des étiquettes des variables, grâce à la règle strlist. Comme j'ai mis la règle str strlist et non la règle strlist str, la tête de la liste est le nom de la fonction, la tête de la queue la première variable, etc... Donc après pour faire ce qu'il faut, je déjoue le piège de daniel hirschkoff qui m'avait dit d'utiliser LIst.fold_left alors qu'il fallait utiliser, vu l'ordre de la liste que je crée List.fold_right.


