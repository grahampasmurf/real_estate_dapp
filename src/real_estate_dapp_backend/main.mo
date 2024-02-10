import Result "mo:base/Result";
import Time "mo:base/Time";
import Hash "mo:base/Hash";
import HashMap "mo:base/HashMap";
import TrieMap "mo:base/TrieMap";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";

actor REDAO {

  type Result<A, B> = Result.Result<A, B>;

  type Role = {
        #Buyer;
        #Seller;
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

    type HashMap<A, B> = HashMap.HashMap<A, B>;

    var nextProposalId : Nat = 0;
    let bids = TrieMap.TrieMap<BidId, Bid>(Nat.equal, Hash.hash);
    let redao : HashMap<Principal, Member> = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);


  public query func greet(name : Text) : async Text {
    return "Hello ICP world, " # name # "!";
  };
};
