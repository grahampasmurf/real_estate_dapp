import Result "mo:base/Result";
import Time "mo:base/Time";
import Hash "mo:base/Hash";
import HashMap "mo:base/HashMap";
import TrieMap "mo:base/TrieMap";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Iter "mo:base/Iter";

actor REDAO {

  type Result<A, B> = Result.Result<A, B>;

  type Role = {
        #Buyer;
        #Agent;
  };

  type Member = {
        name : Text;
        role : Role;
  };

  type ListingStatus = {
    #Active;
    #Inactive;
    #Sold;
  };

    public type ListedProperty = {
        id : Nat;
        MLSnumber : Nat;
        Address: Text;
        Features: Text;
        Picture: Blob; // Picture of the property
        Map: Blob; // Map of the propaerty
        linkToListing: Text;
        creator : Principal; // The member who created the listing
        created : Time.Time; // The time the listing was created
        status : ListingStatus;  // The current status of the listing
    };

    type BidId = Nat;

type Bid = {
        member : Principal; // The member who bid
        bid : Nat; // the amount bid, in a currency whole amount
    };

        public type BidContent = {
        #PropertyId : Nat; // Change the manifesto to the provided text
        #BidAmount : Nat;
        #Buyer : Principal; // Upgrade the member to a mentor with the provided principal 
    };

        public type BidStatus = {
        #Open;
        #Accepted;
        #Rejected;
    };

    type HashMap<A, B> = HashMap.HashMap<A, B>;

    var nextBidId : Nat = 0;
    let bids = TrieMap.TrieMap<BidId, Bid>(Nat.equal, Hash.hash);
    let redao : HashMap<Principal, Member> = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);


  public query func greet(name : Text) : async Text {
    return "Hello! Welcome to the Real Estate DAO, " # name # "!";
  };

// Register a new member in the REDAO with the given name and principal of the caller
    // New members are always Buyer
    // Returns an error if the member already exists
    public shared ({ caller }) func registerMember(name : Text) : async Result<(), Text> {
        switch (redao.get(caller)) {
            case (?member) return #err("Member already exists");
            case (null) {
                redao.put(
                    caller,
                    {
                        name = name;
                        role = #Buyer;
                    },
                );
                return #ok();
            };
        };
    };

    // Get the member with the given principal
    // Returns an error if the member does not exist
    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch (redao.get(p)) {
            case (null) return #err("No member found");
            case (?member) return #ok(member);
        };
    };

    // "Promote" the Buyer with the given principal into an Agent
    // Returns an error if the Buyer does not exist or is not a Buyer
    // Returns an error if the caller is not an Agent
    // Only an Agent can call this function to promote a Buyer to an Agent
    public shared ({ caller }) func becomeAgent(buyer : Principal) : async Result<(), Text> {
        switch (redao.get(caller)) {
            case (?member1) {
                switch (member1.role) {
                    case (#Agent) {
                        switch (redao.get(buyer)) {
                            case (null) return #err("No member found");
                            case (?member2) {
                                switch (member2.role) {
                                    case (#Buyer) {
                                        let newMember = {
                                            name = member2.name;
                                            role = #Agent;
                                        };
                                        redao.put(buyer, newMember);
                                        return #ok();
                                    };
                                    case (#Agent) return #err("Already an Agent");
                                };
                                return #err("You are not a buyer");
                            };
                        };
                    };
                    case (#Buyer) return #err("You are a Buyer; only Agents may do this");
                };
            };
            case (null) return #err("You are not a member");
        };
    };

        func _isMember(p : Principal) : Bool {
        // check if p is member
        switch (redao.get(p)) {
            case (null) return false;
            case (?member) return true;
        };
    };

    func _isAgent(p : Principal) : Bool {
        // check if p is member
        switch (redao.get(p)) {
            case (null) return false;
            case (?member) {
              switch(member.role) {
                case(#Agent) {
                  return true;
                };
                case(#Buyer) {
                  return false;
                };
              };
              return false;
            };
        };
    };

    func _computeBid(oldBid : Nat, newBid: Nat) : Result<Nat, Text> {
        // real code would have to set bounds based on Int size
        if(newBid > oldBid) return #ok(newBid);
        return #err("Your bid was not high enough.");
    };

    // Create a new listing and returns its id
    // Returns an error if the caller is not an Agent
    // UPDATEME 
    public shared ({ caller }) func createBid(content : BidContent) : async Result<BidId, Text> {
        // check if caller is member
        if (not _isMember(caller)) {
            return #err("Not a member");
        };

        let idSaved = nextBidId;
        let bid : Bid = {
            member = caller;
            bid = 0;
        };
        bids.put(idSaved, bid);

        nextBidId += 1;
        return #ok(idSaved);
    };

        // Get the bid with the given id
    // Returns an error if the bid does not exist
    public query func getBid(id : BidId) : async Result<Bid, Text> {
        switch (bids.get(id)) {
            case (null) return #err("Nothing found");
            case (?bid) return #ok(bid);
        };
    };

        // Returns all the bids
    public query func getAllBids() : async [Bid] {
        return Iter.toArray(bids.vals());
    };

};
