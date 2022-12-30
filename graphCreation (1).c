/*-------------------------------------------------------------
Program description: API to read the graph in Compressed Sparse Row(CSR) 
--------------------------------------------------------------*/


#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

struct timeval tt1,tt2;   // for computation time calculation
// graph structure

typedef struct 
{
	int v_count;
	int e_count;
	int* vptr;
	int* eptr;
		
}Graph;

// printing array 



void print_permanence(double* a,int vcount)
{
	printf("\n");
	for(int i = 0;i < vcount;i++)
	printf("%5.3lf ",a[i]);

	return;
}

void print_array(int* arr, int size){
	int i;
	printf("\n");
	for(i = 0;i < size;i++)
	printf(" %d ",arr[i]);

	return;
}

void print1_array(int* arr, int size){
	int i;
	printf("\n");
	printf("Community membership :");
	for(i = 0;i < size;i++)
	printf(" %d ",arr[i]);

	return;
}

Graph* CSR(char* file_name){


	Graph* csr;
	FILE * fp;
	int i=0,j=0;
	int left_v,right_v,prev_v;
	
	csr = (Graph*) malloc(sizeof(Graph));/* Allocate memory for Graph structure */
	if(csr == NULL){
		printf("\nDynamic memory allocation failed.\n");
		return NULL;
	}
	
	/* open input file */
	
	fp = fopen(file_name,"r");
	fscanf(fp,"%d\n%d\n",&csr->v_count,&csr->e_count);/* read no of vertices and edges from the file */
		
	/* Allocate memory for edge pointer array for vertices */
	csr->vptr = (int*) malloc(csr->v_count * sizeof(int));
	
	if(csr->vptr == NULL){
		printf("\nDynamic memory allocation failed.\n");
		return NULL;
	}
	
	/* Allocate memory for edges of the vertices */
	csr->eptr = (int*) malloc(2*csr->e_count * sizeof(int));
	if(csr->eptr == NULL){
		printf("\nDynamic memory allocation failed.\n");
		return NULL;
	}
	
	
	// Reading the first edge of the graph from file 
	fscanf(fp,"%d,%d\n",&left_v,&right_v);
        while(!feof(fp)){
		prev_v = left_v;
		csr->vptr[i] = j;
		// Condition check for multiple edges originating from same vertex
		while(prev_v == left_v && !feof(fp)){
			csr->eptr[j] = right_v;	
			// Reading each edge from file 
			fscanf(fp,"%d,%d\n",&left_v,&right_v);	
			j++;
			// handling the last edge of the graph
			if(feof(fp)){
				if(prev_v != left_v)
					csr->vptr[++i] = j;
				csr->eptr[j] = right_v;
			}
		}
		i++;
	}
	
	fclose(fp); /* close the file */
	return csr;
	
}

int* readcomm(char* file_name,int vcount)
{
	int* community;
	community=(int*)malloc(vcount*sizeof(int));
	if(community==NULL)
	{
		printf("\nDynamic memory allocation failed.\n");
		return NULL;
	}
	int v=0,c=0;
	FILE* fp=fopen(file_name,"r");
	if(fp==NULL)
	{
		printf("\nFile opening failed.\n");
		return NULL;
	}

	while(!feof(fp))
	{
	fscanf(fp,"%d %d\n",&v,&c);
	community[v]=c;
	
    }
	
	return community;
}

int countcommunity(int* community,int vcount) 
{
	int max=-999999;
	for(int i=0;i<vcount;i++)
	{
		if(community[i]>max)
		max=community[i];
	}
	return max;
}



int* degree(Graph* g)
{
	int* degree=(int*)calloc(g->v_count,sizeof(int));
	for(int i=0;i<g->v_count;i++)
	{
		if(i<g->v_count-1)
		degree[i]=g->vptr[i+1]-g->vptr[i];   //for those vertices are not last.
		else{
			degree[i]=(2*g->e_count)-(g->vptr[i]); // for the last vertex.
		}
	}
	return degree;
	}



int* flagarray(int* degreearray,Graph* g,int* community)
{
	int* flagarr=(int*)calloc(g->v_count,sizeof(int));

	for(int i=0;i<g->v_count;i++)
	{
		if(degreearray[i]==1)
		{
			flagarr[i]=1;
		}
	}
		
		for(int j=0;j<g->v_count;j++)
		{
			if(flagarr[j]==0)
			{
				int lim=(j==g->v_count-1)?2*g->e_count:g->vptr[j+1];
				for(int k=g->vptr[j];k<lim;k++)
				{
					int adjvertex=g->eptr[k];
					if(community[j]!=community[adjvertex])
						flagarr[j]=2;
				}
			}
		}
		return flagarr;
} 


Graph* createGraph(char* file_name){
	
	return CSR(file_name);
	
}


double* permanence(Graph* g,int* degreearray,int* community,int countcommunity1,int* flag)
{
	
	gettimeofday(&tt1, 0);
	double* permanence1=(double*)calloc(g->v_count,sizeof(double));
	int *In,*En;
	double p=0.0,s=0.0;
	In=(int*)calloc(g->v_count,sizeof(int));
	En=(int*)calloc(g->v_count,sizeof(int));
	
	for(int i=0;i<g->v_count;i++)
	{
		int lim=(i==g->v_count-1)?2*g->e_count:g->vptr[i+1];
		for(int j=g->vptr[i];j<lim;j++)
		{
			int adj=g->eptr[j];
			if(community[adj]==community[i])
			{
				In[i]+=1;
			}
		}
		En[i]=degreearray[i]-In[i];
	}

int k,count;
	
	for(int i=0;i<g->v_count;i++)
	{
		
		if(flag[i]==2)
		{
			
			k=0,p=0;
			int lim=(i==g->v_count-1)?2*g->e_count:g->vptr[i+1];
			while(k<countcommunity1)
			{
				count=0;
				
				for(int j=g->vptr[i];j<lim;j++)
				{
					int adj=g->eptr[j];
					if(community[adj]!=community[i] && community[adj]==k)
					{
						count++;
					}			
				}

				if(count!=0)
				{
					p+=1.0/(double)count;
				}
				
				
				k++;

			}
			
			permanence1[i]=(p/(double)degreearray[i])*((double)In[i]/(double)En[i]);
			
	    	
		}
	}
    gettimeofday(&tt2, 0);
	double time = (1000000.0*(tt2.tv_sec-tt1.tv_sec) + tt2.tv_usec-tt1.tv_usec)/1000000.0; // calculating the computation time
	
	printf("\nReturning permanence");
	return permanence1;
	
}


// Outlier detection
void Outlier(int flag[],double per[],int vc)
{
	
	
	
	// printf("\n\nPrinting outlier vertices \n\n");

	// printf("Printing outlier nodes with degree 1\n");
	for(int i=0;i<vc;i++)
	{
		if(flag[i]==1)
		printf("%d\n",i);
	}

	printf("\n");
	printf("\n\n---Printing outlier nodes with permanence <= 0.25---\n\n\n");
	for(int j=0;j<vc;j++)
	{
		if(flag[j]==2)
		{
			if(per[j]<=0.25)
			{
				printf("%d\n",j);
			}
		}

	}

	
	
	return;
}




int main(int argc,char* argv[])
{

	Graph* g=createGraph(argv[1]);
	print_array(g->vptr,g->v_count);
	printf("\n\n");
	print_array(g->eptr,2*g->e_count);
	printf("\n\n");
	int* community=readcomm(argv[2],g->v_count);
	print1_array(community,g->v_count);
	printf("\n\n\nThe maximum number of communities is %d\n",countcommunity(community,g->v_count)+1);
	int* degreearray=degree(g);	
	
	printf("\n\nThe degree of the vertices are : ");
	print_array(degreearray,g->v_count);

	int* fl=flagarray(degreearray,g,community);
	printf("\n\nThe flag array is as follows : ");
	print_array(fl,g->v_count);

	int x=countcommunity(community,g->v_count);


	double* perm=permanence(g,degreearray,community,x+1,fl);
	print_permanence(perm,g->v_count);

	Outlier(fl,perm,g->v_count);
	
	
return 0;


}

