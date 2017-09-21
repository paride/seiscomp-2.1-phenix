/*
 *    int     tGetIndex(TITFILE *fp, int nindex, TITINDEX *index)
 * tGetIndex:
 * Lecture des index de declenchement associes a un fichier titan
 *
 * entree:
 *    fp= pointeur fichier titan
 * sortie:
 *    index= tableau d'index
 *    nindex= nombre d'index max a lire
 * globale:
 *    Fp_log= pointeur sur fichier erreur
 * retour:
 *    nombre d'index lus en cas de succes, 0 sinon
 */

int tGetIndex(TITFILE *fp,int nindex, TITINDEX *index)
{
extern    FILE    *Fp_log;

    switch (fp->type) {
    case TFILE:
        fprintf(Fp_log,"tGetIndex: error: no indexes in titan files\n");
        return(0);
#ifndef DOS
    case TDAT:
#ifdef ST_HAS_PARTITION
        return(dat_getIndex(fp,nindex,index));
#else
        fprintf(Fp_log,"tGetIndex: error: dat drive does not allow index reading\n");
        return(0);
#endif /* ST_HAS_PARTITION */
#endif /* DOS */
    case TDISK:
        return(disk_getIndex(fp,nindex,index));
    default:
        return(0);
    }
}
