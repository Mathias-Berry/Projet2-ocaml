Nonjour bonjour, cette fois-ci, Le grand Emile a résout les conflits alors que Mathias les a générés.
Si l'on devait résumer, le fichier type.ml a été fait par Mathias, le main, resoud.ml et les tests par Emile ( même si on s'est aidé mutuellement dans chaque )


Pour la génération des conflits Mathias a utiliser une variable global contraintes étant une ref (int*type) list. Si (i,t) est dans !contraintes alors le type i dois pouvoir s'unifier avec le type t. Pour ajouter les conflits a cette référence Mathias utilise une fonction récursive "typage" qui prend en argument un environnement contenant les variables ainsi que leurs type et une expression. on reviendrai sur le fonctionnement de typage après. Comme d'habitude on utilise quelque fonction "recup". Ensuite Mathias utilise trois fonctions liées au motif. La première est "addmotenv" qui étant donné un motif renvoit une suite contenant le fait que chaque variable du motif soit attribué a un nouveau type. ensuite, Mathias utilises "typagemot" qui étant donné un motif renvoit le type du motif (si on a que x est de type int et y bool typagemot (Tuplem [x,y]) renvoit Tuples [int,bool]). Et enfin typagemot qui prend en argument un motif est un type, et qui renvoit une liste qui contient chaque variable associé au type qu'elle doit avoir. Cette drenière fonction ajoute aussi les variables du motif à la variable global var (qui contient ce que le -showtypes doit renvoyer).
Une fois que ceci est vu on va s'interreser à comment fonctionne typage. Déjà typage permet d'ajouter les contraintes mais elle renvoit aussi le type de l'expression, ce qui me sera très utile. On commence par matcher l'expression. Ensuite, il est important de remarquer que tout le long on définit des varibles temporaires qui correspondent aux appels récursif car on doit faire attention avec l'incrémentation de cota. Pour ajouter la contrainte que les types  t1 et t2 soit égaux on doit rajouter a mes contraintes que la constantes i soit du types t1 et t2. On va donc incr cota; append (!cota,t1); append (!cota,t2);. Une fois vu cela il faut aller dans les détails:
Pour toutes les opérations classique ou les constante il n'y a pas grand chose a redire cela parait plutot simple en aillant pris connaiçance de ce qui a était dit avant.
La première subtilité arrive au letin. Ce qu'il faut faire c'est utiliser la fonction typagemot avec le motif et l'appel récurcif sur ce que vaut e1 pour unifier les types des variables avec ce qu'elles doivent valoir. Et on ajoute ceci a l'environnement avant d'appeler le derière appele sur e2.
Le letrec fonctionne de la même manière que le letin a la différence près que l'on doit ajouter la variable qu'on est en train de typer dans l'environnement. Et penser a unifier sont type avec le retour de sa propore évaluation.
Ensuite les difficultés arrivent pour le typage d'une fonction. Pour cela j'utilise la fonction addmotenf sur m pour ajouter les variables de la fonction dans l'environement avant de l'évaluer. Ensuite on génère les contraintes du a son body puis on récupère le type du motif avec recomposemot afin de renvoyer une fonction du bon type.
Ensuite le prochain problème est l'application d'une fonction. Pour on ajoute la contrainte : type de la fonction = Fonc(type de ce a quoi on applique la fonction,'a).
Ensuite le problème est le Match avec temp3 et temp4, on ajoute la contrainte que ce que l'on match puisse s'unifier avec les différent sous-cas et avec temp1 et temp2 on ajoute la contrainte que tout les matchs soit bien du même type. On utilise la même méthode que dans les fonctions pour garder connu le type des variable uniquement dans le "body" du match.
Et la dernière chose de cette fonction est le try que l'on règle comme un match pour unifier les retour au détail près du cas d'attêt. Lors du cas d'arrêt le try doit renvoyer quelque chose du même type que ce que l'on essaye de faire et non un 'a classique.

L'utilisation de la partie se résume a utiliser la fonction "génère" qui appelle typage et renvoit les contraintes ainsi que les variables utilisées et le numéro de la dernière contrainte engendrée.




A partir de là c'est Emile qui parle.


Dans le main, pas grand chose à dire, hormis eventuellement pour l'affichage qui utilise simplement la liste des variables et leur types pour que je les affiche.
Dans resoud.ml, la strucure n'est pas très compliquer. Dans la fonction resolution, le tableau encours stocke des types, qui initialement sont à Tout, c'est à dire l'équivalent de 'a. Grace au Pasdef qui peut pointer vers d'autre case, on a en quelques sortes une structure d'union find où le représentant et le premier type qui n'est pas un pointeur vers autre chose (aka un Pasdef). Puis on parcoure les contraintes une par une ( à travers la fonction aux ). Dès le début, on prend le représentant dans la structure union find de l'élément du tableau dans lequel on est, puis on regarde si on peut l'unifier, sinon on soulève l'exception erreur. Si on doit pour unifier, tester l'égalité entre deux types, on va prendre la première case non utilisée (grace au pointeur bout), puis recommencer en rajoutant la contrainte que cette case ( qui contient Tout aka 'a ) doit pouvoir être unifier avec les types. Cela fait qu'on peut avoir besoin d'utiliser un nombre exponentiel de case, sans connaitre ce nombre avant, ce qui est problématique mais c'est la vie.

Quand il y a une erreur de type, je laisse l'exception Resoud.erreur, car pour la moulinette, les programmes mal typé je les mets dans Shouldfail, et par conséquent il me faut une erreur en sortie.

Pour afficher proprement les types, j'ai une référence qui me sert à me demander quelles lettres j'ai déjà utilisées, pour passer à la suivante.

Le programme let rec f x = x in f f ne plante pas, mais si on met l'option showtypes, la il va pas y arrivé car il va vouloir tout parcourir récursvement, et donc tourné en boucle et donc afficher n'importe quoi.




