# Rapport TP

## Travail effectué
J'ai rempli les fichiers demandé afin de simuler l'execution d'un netfile en ocaml. 

## Fonctionnement du simulateur
Le simulateur passe par plusieurs grandes étapes
### 1. Ordonnancement
Les Netfile peuvent parfois avoir des instructions dépendantes les unes des autres. Ainsi, il est souvent nécéssaire de réordonner les instructions afin qu'elles s'éxecutent dans le bonne ordre dans le simulateur, afin de bien émuler un cycle du
processeur. Pour cela, on code une fonction qui permet un tri topologique des expressions. Il faut bien faire attention au cas de la RAM et de la ROM: on ne dépend que de read_adress. 

### 2. Initialisation de la RAM et ROM 
Il faut initialiser les RAMSs et les ROMs utiliser par le programme. Voir la section Conventions pour comprendre ce choix. Il faut également initialiser les registres.

### 3. Executions des opérations
On execute dans l'odre renvoyer par le tri topologique les operations du cycle. Attention, quand on a une instruction RAM a cet instant on ne fait que l'opération de lecture. 

### 4. Ecriture dans les RAMS
On peut maintenant ecrire dans les RAMS si il y a besoin de le faire. 

### 5. Fin de cycle
On affiche les resultats des sortie ayant été étiqueté comme output puis on met a jour les registres pour le cycle suivant. 

## Conventions
Le sujet laissant disons quelques libertés, j'ai choisit d'établir certaines conventions que mon simulateur respecte
### 1. Initialisation de la mémoire
J'ai choisi de mettre 0 comme valeur de base dans les registres ainsi que dans les RAMs. Dans les ROMs, si une instruction de la forme name = ROM ... est appelé, il doit exister un fichier "name.db" qui donne le contenu de la ROM ligne par ligne
au début de la simulation. 
### 2. Représentation de la mémoire
J'ai choisit d'avoir plusieur RAMs et ROMs. Ainsi, il y a 3 tables de hashage pour la mémoire: celle des registres, celle des RAMs et celle des ROMs. Elle sont a chaque fois identifié par leur nom (donc un ident). 
### 3. Mise à jour de la mémoire
Comme préciser plus tot, on ecrit dans les registres et dans la RAM qu'en fin de cycle, après avoir imprimé les outputs demandés.

## Compilation 
Taper d'abord 
```console
ocamlbuild netlist_simulator.byte
```

Puis 
```console
./netlist_simulator.byte path_to_test
```