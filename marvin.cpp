#include<bits/stdc++.h>

#define positionMap vector<pair<int, int> >
using namespace std;

/*

Marvin cipher
-------------

Credit to DeinFreund for the idea

 */



// inverting the row/column to character arrangement for quick lookup
positionMap calculatePositionMap(const vector<string> & arrangement){ // {x, y}
    positionMap out (26);
    for(size_t y = 0; y < arrangement.size(); ++y){
	for(size_t x = 0; x < arrangement[y].length(); ++x){
	    char c = arrangement[y][x];
	    out[c - 'a'] = {x, y};
	}
    }
    return out;
}

// calculate the keyboard distance between two chars
int calculateDistance(const positionMap & map, int rowDist, char a, char b){
    auto posA = map[a - 'a'];
    auto posB = map[b - 'a'];

    int dx = abs(posB.first - posA.first);
    int dy = abs(posB.second - posA.second);

    return dx + rowDist * dy;
}


bool isLowercaseLetter(char c){
    return 'a' <= c && c <= 'z';
}

// given a keyboard position, return a list of possible positions at a given distance
vector<pair<int, int> > getPositionsAtDistance(pair<int, int> pos, vector<int> dimensions,
					      int rowDist, int dist){
    int height = dimensions.size();

    int x = pos.first;
    int y = pos.second;

    vector<pair<int, int> > possiblePlaces;
    
    for(int i = dist; i >= 0; i-=rowDist){
	possiblePlaces.push_back({ x-i, y + (dist-i)/rowDist });

	if(i != 0){
	    possiblePlaces.push_back({ x+i, y + (dist-i)/rowDist });
	}
	
	if(i != dist){
	    possiblePlaces.push_back({ x-i, y - (dist-i)/rowDist });

	    if(i != 0){
		possiblePlaces.push_back({ x+i, y - (dist-i)/rowDist });
	    }
	}
    }

    vector<pair<int, int> > out;

    for(auto p : possiblePlaces){
	int x = p.first;
	int y = p.second;
	if( y >= 0 && y < height && x >= 0 && x < dimensions[y]){
	    out.push_back(p);
	}
    }
    
    return out;
}


// inverse the letter frequency mapping from int->char to char->int for easy lookup
vector<int> getLetterFrequencyPositions(const string & letterFreq){
    int n = letterFreq.length();
    
    vector<int> out (n, 0);

    for(int i = 0; i < n; ++i){
	out[letterFreq[i] - 'a'] = i;
    }
    return out;
}

string encode(const vector<string> & keyboardArrangement,
	      int rowDist, int charSplit,
	      const string & letterFreq, const string & input){

    string out = "";

    pair<int,int> pos = {0, 1};
    bool firstLetterSeen = false;

    vector<int> letterFreqPos = getLetterFrequencyPositions(letterFreq);
    
    vector<int> dimensions;
    for(auto r:keyboardArrangement){
	dimensions.push_back(r.length());
    }
    
    for(auto c:input){
	if(isLowercaseLetter(c)){
	    if(! firstLetterSeen){
		firstLetterSeen = true;
		out += "a";
	    }

	    int dist = letterFreqPos[c - 'a'];

	    while(dist >= charSplit){
		pos = getPositionsAtDistance(pos, dimensions, rowDist, charSplit)[0];
		out += keyboardArrangement[pos.second][pos.first];
		dist -= charSplit;
	    }

	    pos = getPositionsAtDistance(pos, dimensions, rowDist, dist)[0];
	    out += keyboardArrangement[pos.second][pos.first];
	    
	}else{
	    out+= c;
	}
    }
    
    return out;
}



string decode(const vector<string> & keyboardArrangement,
	      int rowDist, int charSplit,
	      const string & letterFreq, const string & input){

    positionMap map = calculatePositionMap(keyboardArrangement);
    
    string out = "";
    
    bool firstLetterSeen = false; // has the first letter already been passed
    char lastLetter;
    int partialSum = 0;
    for(auto c:input){
	if(isLowercaseLetter(c)){
	    if(firstLetterSeen){
		int dist = calculateDistance(map, rowDist, lastLetter, c);
		partialSum += dist;
		if(dist < charSplit){
		    out+= letterFreq[partialSum];
		    partialSum = 0;
		}
	    }else{
		firstLetterSeen = true;
	    }
	    lastLetter = c;
	}else{
	    out += c;
	}
    }

    return out;
}



int main(){
    vector<string> deLayout = {"qwertzuiop", "asdfghjkl", "yxcvbnm"};
    vector<string> ukLayout = {"qwertyuiop", "asdfghjkl", "zxcvbnm"};

    string characterFrequency = "etaoinsrhldcumfpgwybvkxjqz";

    //string message = "ahlbtr krtt afhb ldjnvrtlkrvn kvaadumk ie cfstlaqtf";
    string message = "ahdmds nsaa lhfl ajdkhxygfifs kvaadmdk fl ahfygybyn";
    
    cout<< decode(deLayout, 4, 8, characterFrequency, message) << endl;

    string plaintext = "now we can finally speak in marvin";

    cout<< encode(deLayout, 4, 8, characterFrequency, plaintext) << endl;
}

