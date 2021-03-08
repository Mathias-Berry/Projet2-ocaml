On a fait ensemble la partie débutante moins les options.

Ensuite, Emile a fait la partie lexer et la partie parser, plus l'ajout des options et du fait de devoir donner en entrée un fichier sans les chevrons. J'ai aussi conformé l'affichage d'un code à ce que Caml peut accpeter.
Mathias quant à lui a fait tous le code d'eval et de expr. Je me suis chargé de changer le nom de main.native et de comprendre et résoudre les problèmes des tests.


Pour la partie lexer, j'ai juste rajouté ce qu'il manquait, rien de spécial à ajouter.
Pour le parser, j'ai découpé les expressions en deux types, les atomiques ( variables, constantes et machin entre parenthèses ), et le reste ( sachant que dans le fond tout est expression, y compris les booléens ). Ainsi, l'application d'une fonction se detecte par la règle expression atomique, puisque c'est le seul cas ou on peut se retrouver dans cette situation, avec le cas de la règle contenant Uminus. D'où l'ajout de token imaginaire pour ordonner leur priorité. Pour les let in, les fonction et prInt, il faut que l'expression qu'il y a après le let in ou la flèche de la fonction soit la plus grande possible, et donc j'ai donner la priorité minimale à ces règles.
Ensuite, je donne la priorité maximale à la règle aqui renvoie vers atomique car dès qu'on tombe sur une expression qui pourrait être une expression atomique, hop on la reduit.
Ensuite, je ne sais pas pourquoi, il a fallu donner des priorités aux règles de atomique:. De ce que j'avais lu, j'avais déduit qu'il y en avait pas besoin, mais quand je les mets pas il y a des conflits, donc je les mets.
Enfin, pour les fonction définies comme suit : let f x y z = ... alors je récupère la liste des étiquettes des variables, grâce à la règle strlist. Comme j'ai mis la règle str strlist et non la règle strlist str, la tête de la liste est le nom de la fonction, la tête de la queue la première variable, etc... Donc après pour faire ce qu'il faut, je déjoue le piège de daniel hirschkoff qui m'avait dit d'utiliser LIst.fold_left alors qu'il fallait utiliser, vu l'ordre de la liste que je crée List.fold_right.



Pour le type des exprs j'ai réalisé un type arithop permettant de mettre de cotès toute les opérations arihmétiques. Ce qui permet d'ajouter des opérations très facilement. J'ai fait la même chose pour les opération renvoyant des bolléens, sauf que je l'ai ai différencier celon si elles prennent des boolléens (boolop2) en arguments ou des entiers (boolop1). Ce qui est plus pratique pour l'évaluation. Il n'y a rien à dire de particulier sur l'affichage.

Pour l'évaluation : 
J'ai créer deux types, un type environnement contennant une list de couples de string valeur (c'est un dictionnaire) et un type valeur étant soit un entier, soit un bool ou soit une fonction que je représente par 4 élément. Le premier est l'environnemant au moment ou la fonction a été créee (pour pouvoir l'évalué même si des variables ont changé de valeur depuis), le second élément est la varible de la fonction (pour fun x -> f(x) c'est x), le troisème est le body de la fonction (dans le même exemple c'est f(x)) est le dernier elément est le nom de la fonction si la fonction est récursive, sinon c'est None (j'y reviendrai lors de l'évaluatuion).
Au début j'ai plein de fonctions recup... qui permettent de pour un élément qui de la forme Int x ou Var x... de me renvoyer x. Et une fonction recup qui permet de récupérer la valeur d'une variable dans un environnement.
Pour la fonction eval en elle même, je n'ai pas grand chose à dire sur les 7 premiers cas traité si ce n'est que je fais des fonction pour créer des fonctions a partir des boolop ou arithop. Sinon je faisce qui est évident.
Pour le cas ou j'ai un letin je teste si la ce que je rajoute est une fonction pour savoir comment je l'ajoute dans l'environnement (en tant que Int ou Fun).
Pour le letrec, on a considerer que cela ne s'appliquer que au fonction (même si ocaml n'aplique pas forcement ça). Sinon j'aurai fais casiment le même code que pour letin et un let rec sur un entier ne change rien du coup je n'aurais pas fait de différence.
Pour le print j'ai juste regardé si on voulait le fair (si on fait un -showsrc on affiche pas les print).
Pour l'application :
	Je commence par regarder si le première élément est une fonction, en ce cas j'évalue le body de la fontion dans l'environnement au quel j'ai ajouté le fait que la variable de la fonction soit attribué à la valeur de de ce a quoi in l'applique.
	Ensuite si je distingue 2 cas, si ce que j'ai avant est une application ou une variable. Dans les deux cas je récupère cette elémént sous la forme d'une valeur Fun. Je traite d'abord les fonctions non recursive. Pour cela je fais environs la même chose sauf que j'évalu le body dans l'environnement dans lequel a était créé la fonction au quel j'ai ajouté le fait que la variable soit associé a la valeur dans l'environnement actuel de ce a quoi on applique la fonction.
	Enfin si la fonction est recursive je fais la même chose que dans le cas 2 mais j'ajoute a l'environement dans lequel on évalu le body la fonction elle même avant la valeur.

