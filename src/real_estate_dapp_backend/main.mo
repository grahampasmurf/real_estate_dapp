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

    type PropertyId = Nat;
    type ListedProperty = {
        id : Nat;
        mls : Nat;
        address : Text;
        Features : Text;
        // Picture : Blob; // Picture of the property
        // Map : Blob; // Map of the propaerty
        // linkToListing : Text;
        creator : Principal; // The member who created the listing
        created : Time.Time; // The time the listing was created
        status : ListingStatus; // The current status of the listing
        highestBid : Nat;
        highestBidder : ?Principal; // allow null
    };

    type HashMap<A, B> = HashMap.HashMap<A, B>;

    var nextPropertyId : Nat = 0;
    let properties = TrieMap.TrieMap<PropertyId, ListedProperty>(Nat.equal, Hash.hash);
    let redao : HashMap<Principal, Member> = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);

    public query func greet(name : Text) : async Text {
        return "Hello! Welcome to the Real Estate DAO, " # name # "!";
    };

    // Register a new member in the REDAO with the given name and principal of the caller
    // New members are always Buyer
    // the very first member becomes an Agent, and can then promote others to Agents
    // Returns an error if the member already exists
    public shared ({ caller }) func registerMember(name : Text) : async Result<(), Text> {
        switch (redao.get(caller)) {
            case (?member) return #err("Member already exists");
            case (null) {
                if (redao.size() == 0) {
                    // if the REDAO is size 0 (no members) then this first member will be an Agent
                    redao.put(
                        caller,
                        {
                            name = name;
                            role = #Agent;
                        },
                    );
                    return #ok();
                };
                // else add as a Buyer
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
                switch (member.role) {
                    case (#Agent) {
                        return true;
                    };
                    case (#Buyer) {
                        return false;
                    };
                };
                return false;
            };
        };
    };

    // create and get properties
    // Create a new listing and returns its id
    // Returns an error if the caller is not an Agent
    // UPDATEME
    public shared ({ caller }) func createProperty(address : Text, MLS : Nat) : async Result<PropertyId, Text> {
        // check if caller is member
        if (not _isMember(caller)) {
            return #err("Not a member");
        };

        // only Agents can list a property
        if (not _isAgent(caller)) return #err("Only Agents can create a Property Listing.");

        let idSaved = nextPropertyId;
        let newProperty : ListedProperty = {
            id = idSaved;
            creator = caller;
            mls = MLS;
            Features = "";
            address = address;
            created = Time.now();
            highestBid = 0;
            highestBidder = null;
            status = #Active;
        };
        properties.put(idSaved, newProperty);

        nextPropertyId += 1;
        return #ok(idSaved);
    };

    // Bid for the given property
    // Returns an error if the property does not exist or the bid is not the highest bid
    public shared ({ caller }) func bidOnProperty(propertyId : PropertyId, bid : Nat) : async Result<(), Text> {
        if (not _isMember(caller)) {
            return #err("Not a member; cannot bid");
        };
        switch (properties.get(propertyId)) {
            case (null) return #err("Property not found");
            case (?property) {
                if (property.status == #Inactive or property.status == #Sold) return #err("Property is not available.");
                // check if already highest bidder
                switch (property.highestBidder) {
                    case (caller) return #err("Already highest bidder.");
                };
                // passed all checks
                if (property.highestBid >= bid) {
                    let bidText = Nat.toText(bid);
                    let highestBidText = Nat.toText(property.highestBid);
                    return #err("Your bid of " # bidText # " did not exceed the highest bid of " # highestBidText # "!");
                };
                // bid is high enough
                let newProperty : ListedProperty = {
                    id = propertyId;
                    creator = property.creator;
                    mls = property.mls;
                    Features = "";
                    address = property.address;
                    created = property.created;
                    highestBid = bid;
                    highestBidder = ?caller;
                    status = #Active;
                };
                properties.put(property.id, newProperty);
                return #ok();
            };
        };
    };

    public shared ({ caller }) func acceptHighestBidOnProperty(propertyId : PropertyId) : async Result<(), Text> {
        // check if caller is member
        if (not _isMember(caller)) {
            return #err("Not a member");
        };

        // only Agents can list a property
        if (not _isAgent(caller)) return #err("Only Agents can accept a bid.");

        switch (properties.get(propertyId)) {
            case (null) return #err("Property not found");
            case (?property) {
                let newProperty : ListedProperty = {
                    id = propertyId;
                    mls = property.mls;
                    Features = "";
                    address = property.address;
                    created = property.created;
                    creator = property.creator;
                    highestBid = property.highestBid;
                    highestBidder = property.highestBidder;
                    status = #Sold;
                };
                properties.put(propertyId, newProperty);

                return #ok();
            };
        };
    };

    public shared ({ caller }) func deactivatePropertyListing(propertyId : PropertyId) : async Result<(), Text> {
        // check if caller is member
        if (not _isMember(caller)) {
            return #err("Not a member");
        };

        // only Agents can deactivate a property
        if (not _isAgent(caller)) return #err("Only Agents can deactivate a property.");

        switch (properties.get(propertyId)) {
            case (null) return #err("Property not found");
            case (?property) {
                let newProperty : ListedProperty = {
                    id = propertyId;
                    mls = property.mls;
                    Features = "";
                    address = property.address;
                    created = property.created;
                    creator = property.creator;
                    highestBid = property.highestBid;
                    highestBidder = property.highestBidder;
                    status = #Inactive;
                };
                properties.put(propertyId, newProperty);

                return #ok();
            };
        };
    };

};
