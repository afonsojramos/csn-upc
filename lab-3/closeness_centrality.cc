#include <iostream>
#include <fstream>
#include <vector>
#include <queue>
#include <limits>
#include <string>

using namespace std;

typedef struct{
    unsigned int id = -1;
    vector< unsigned int> adj;
} Node;

void bfs(unsigned int first_node, vector< Node> &graph, vector< unsigned int > &out,vector< bool > &visited){ 

    queue<unsigned int> to_visit, dist;
    visited[first_node] = true; 
    to_visit.push(first_node);
    dist.push(0);
  
  
    while(!to_visit.empty()) { 
        unsigned int v = to_visit.front();
        unsigned int d = dist.front();
        out[v]=d;
        to_visit.pop();
        dist.pop();

        for (int i = 0; i < graph[v].adj.size(); ++i){

            unsigned int i_node = graph[v].adj[i];
            if (!visited[i_node]){ 
                visited[i_node] = true;
                to_visit.push(i_node);
                dist.push(d+1);
            } 
        } 
    } 
}

void remove_ones(vector< Node> &graph, vector< Node > &removed_graph, vector< bool > &removed){ 
    for(int i = removed_graph.size()-1; i>=0; --i){
        if(graph[i].adj.size()==1){
            removed[i]=true;
            unsigned int connected = graph[i].adj[0];
            graph[i].adj.erase(graph[i].adj.begin());
            removed_graph[connected].adj.push_back(i);
            removed_graph[i].adj.push_back(connected);
            if(connected<i){
                int j=0;
                bool found = false;
                while(j<graph[connected].adj.size() and not found){
                    if(graph[connected].adj[j]==i){
                        graph[connected].adj.erase(graph[connected].adj.begin()+j);
                        found=true;
                    }
                    ++j;
                }
            }
        }
        for(int j=0;j<graph[i].adj.size();++j){
            if(removed[graph[i].adj[j]]){
                graph[i].adj.erase(graph[i].adj.begin()+j);
            }
        }

    }
}

int main(int argc, char *argv[]){
    // Read the graph
    if(argc!=2 and argc!=4){
        cout << "Wrong number of arguments" << endl;
        return -1;
    }
    int n, e;
    ifstream infile(argv[1]);

    if(argc==2) infile >> n >> e; //if not n and e not passed as arguments, we assume they are at the start of the file
    else if(argc==4){ //otherwise we read them from the argument
        n = stoi(argv[2]);
        e = stoi(argv[3]);
    }

    vector< Node > graph(n);
    for(int i=0; i<n; ++i) graph[i].id=i;

    for(int i=0; i<e; ++i){
        unsigned int u,v;
        infile >> u >> v;
        graph[u].adj.push_back(v);
        graph[v].adj.push_back(u);
    }
    // cout<<"Correct Read"<<endl;

    // Remove single nodes
    vector< Node > removed_graph(n);
    vector< bool > removed(n,false);
    for(int i=0; i<n; ++i) graph[i].id=i;
    remove_ones(graph,removed_graph,removed);
    // cout<<"Correct Remove"<<endl;

    // Get Closeness Centrality
    float C = 0;
    #pragma omp parallel for reduction(+:C)
    for(int i = 0; i < n; ++i){
        vector< unsigned int > d(n,n);
        vector< bool > visited(n,false);
        bfs(i,graph,d,visited);
        // int not_visited = 0;
        float Ci = 0;
        for(int j = 0; j < n; ++j){
            // if(not visited[j]) ++not_visited;
            // else if(j!=i) Ci+=1.0/d[j];
            if(visited[j] and j!=i) Ci+=1.0/d[j];
            else if(removed[j]) Ci+=1.0/(d[removed_graph[j].adj[0]]+1);
        }
        for(int k = 0; k < removed_graph[i].adj.size(); ++k){
            for(int j = 0; j < n; ++j){
                // if(not visited[j]) ++not_visited;
                // else if(j!=i) Ci+=1.0/d[j];
                if(j!=k){
                    if(visited[j]) Ci+=1.0/(d[j]+1);
                    else if(removed[j]) Ci+=1.0/(d[removed_graph[j].adj[0]]+1);
                }
            }
        }
        C+=Ci/(n-1);
    }
    C=C/n;
    cout << C << endl;
}