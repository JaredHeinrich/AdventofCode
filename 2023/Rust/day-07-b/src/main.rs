use std::{collections::HashMap, fs, u32, u8};

fn main() {
    let input = fs::read_to_string("../../inputs/input-07.txt").unwrap();
    let mut hands: Vec<Hand> = input.lines().map(|line| {
        let mut split = line.split(" ");
        let cards: Deck = split.next().unwrap().chars().map(|c| Card::from_char(c).unwrap()).collect::<Vec<Card>>().try_into().unwrap();
        let bid: u32 = split.next().unwrap().parse().unwrap();
        Hand{
            cards,
            bid,
        }
    })
    .collect();
    hands.sort();
    let mut result = 0;
    for (i,hand) in hands.iter().enumerate(){
        result += (i as u32 +1)*hand.bid;
    }
    println!("result = {}", result);
}

type Deck = [Card;5];

#[derive(Debug)]
struct Hand{
    cards: Deck,
    bid: u32,
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Hash, Clone, Copy)]
enum Card {
    Ace = 12,
    King = 11,
    Queen = 10,
    Ten = 9,
    Nine = 8,
    Eight = 7,
    Seven = 6,
    Six = 5,
    Five = 4,
    Four = 3,
    Three = 2,
    Two = 1,
    Jocker = 0,
}

impl Card{
    fn from_char(c: char) -> Option<Self>{
        match c {
            'A' => Some(Card::Ace),
            'K' => Some(Card::King),
            'Q' => Some(Card::Queen),
            'J' => Some(Card::Jocker),
            'T' => Some(Card::Ten),
            '9' => Some(Card::Nine),
            '8' => Some(Card::Eight),
            '7' => Some(Card::Seven),
            '6' => Some(Card::Six),
            '5' => Some(Card::Five),
            '4' => Some(Card::Four),
            '3' => Some(Card::Three),
            '2' => Some(Card::Two),
            _ => None,
        }
        
    }
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord)]
enum Type{
    FiveOfAKind = 6,
    FourOfAKind = 5,
    FullHouse = 4,
    ThreeOfAKind = 3,
    TwoPairs = 2,
    OnePair = 1,
    HighCard = 0,
}

impl PartialEq for Hand{
    fn eq(&self, other: &Self) -> bool {
        self.cards == other.cards
    }
    fn ne(&self, other: &Self) -> bool {
        self.cards != other.cards
    }
}



impl PartialOrd for Hand{
    fn lt(&self, other: &Self) -> bool {
        match self.partial_cmp(other) {
            Some(std::cmp::Ordering::Less) => true,
            Some(std::cmp::Ordering::Equal) => false,
            Some(std::cmp::Ordering::Greater) => false,
            None => false,
        }
    }

    fn le(&self, other: &Self) -> bool {
        match self.partial_cmp(other) {
            Some(std::cmp::Ordering::Equal) => true,
            Some(std::cmp::Ordering::Less) => true,
            Some(std::cmp::Ordering::Greater) => false,
            None => false,
        }
    }

    fn gt(&self, other: &Self) -> bool {
        match self.partial_cmp(other) {
            Some(std::cmp::Ordering::Greater) => true,
            Some(std::cmp::Ordering::Equal) => false,
            Some(std::cmp::Ordering::Less) => false,
            None => false,
        }
    }

    fn ge(&self, other: &Self) -> bool {
        match self.partial_cmp(other) {
            Some(std::cmp::Ordering::Equal) => true,
            Some(std::cmp::Ordering::Greater) => true,
            Some(std::cmp::Ordering::Less) => false,
            None => false,
        }
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let type_self = get_type(&self.cards);
        let type_other = get_type(&other.cards);

        match type_self.partial_cmp(&type_other) {
            Some(std::cmp::Ordering::Greater) => Some(std::cmp::Ordering::Greater),
            Some(std::cmp::Ordering::Less) => Some(std::cmp::Ordering::Less),
            Some(std::cmp::Ordering::Equal) => {
                for i in 0..self.cards.len() {
                    match self.cards[i].partial_cmp(&other.cards[i]) {
                        Some(std::cmp::Ordering::Equal) => continue,
                        Some(std::cmp::Ordering::Greater) => return Some(std::cmp::Ordering::Greater),
                        Some(std::cmp::Ordering::Less) => return Some(std::cmp::Ordering::Less),
                        None => return None,
                    }
                }
                Some(std::cmp::Ordering::Equal)
            },
            None => None
        }
    }
}

impl Eq for Hand{}
impl Ord for Hand{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let type_self = get_type(&self.cards);
        let type_other = get_type(&other.cards);

        match type_self.cmp(&type_other) {
            std::cmp::Ordering::Greater => std::cmp::Ordering::Greater,
            std::cmp::Ordering::Less => std::cmp::Ordering::Less,
            std::cmp::Ordering::Equal => {
                for i in 0..self.cards.len() {
                    match self.cards[i].cmp(&other.cards[i]) {
                        std::cmp::Ordering::Equal => continue,
                        std::cmp::Ordering::Greater => return std::cmp::Ordering::Greater,
                        std::cmp::Ordering::Less => return std::cmp::Ordering::Less,
                    }
                }
                std::cmp::Ordering::Equal
            },
        }
        
    }
}

fn get_type(cards: &Deck) -> Type {
    let mut cards_map: HashMap<Card,u8> = HashMap::new();
    let mut map: HashMap<u8,u8> = HashMap::new();
    for card in cards.iter(){
        cards_map.entry(*card).and_modify(|e| *e+=1).or_insert(1);
    }
    let jocker: u8 = cards_map.remove(&Card::Jocker).unwrap_or(0);
    for (_,v) in cards_map.iter(){
        map.entry(*v).and_modify(|e| *e+=1).or_insert(1);
    }
    map.insert(0, 0);
    if map.contains_key(&(5-jocker)) {
        return Type::FiveOfAKind;
    }
    if map.contains_key(&(4-jocker)) {
        return Type::FourOfAKind;
    }
    if (map.contains_key(&3) && map.contains_key(&2)) || (jocker >= 1 && map.contains_key(&2) && *map.get(&2).unwrap() == 2){
        return Type::FullHouse;
    }
    if map.contains_key(&(3-jocker)) {
        return Type::ThreeOfAKind;
    }
    if *map.get(&2).unwrap_or(&0) == 2 {
        return Type::TwoPairs;
    }
    if map.contains_key(&(2-jocker)) {
        return Type::OnePair;
    }
    Type::HighCard
}

#[cfg(test)]
mod test{
    use crate::{get_type, Card, Type};

#[test]
    fn test_get_type_five_of_a_kind() {
        let cards: [Card;5] = [Card::Ace,Card::Ace,Card::Ace,Card::Ace,Card::Ace];
        assert_eq!(Type::FiveOfAKind, get_type(&cards));
    }
#[test]
    fn test_get_type_four_of_a_kind() {
        let cards: [Card;5] = [Card::King,Card::Ace,Card::Ace,Card::Ace,Card::Ace];
        assert_eq!(Type::FourOfAKind, get_type(&cards));
    }
#[test]
    fn test_get_type_full_house() {
        let cards: [Card;5] = [Card::Two,Card::Two,Card::Ace,Card::Ace,Card::Ace];
        assert_eq!(Type::FullHouse, get_type(&cards));
    }
#[test]
    fn test_get_type_three_of_a_kind() {
        let cards: [Card;5] = [Card::Two,Card::King,Card::Ace,Card::Ace,Card::Ace];
        assert_eq!(Type::ThreeOfAKind, get_type(&cards));
    }
#[test]
    fn test_get_type_two_pairs() {
        let cards: [Card;5] = [Card::Ace,Card::Ace,Card::Seven,Card::Queen,Card::Queen];
        assert_eq!(Type::TwoPairs, get_type(&cards));
    }
#[test]
    fn test_get_type_one_pair() {
        let cards: [Card;5] = [Card::Queen,Card::King,Card::Ace,Card::Ace,Card::Ten];
        assert_eq!(Type::OnePair, get_type(&cards));
    }
#[test]
    fn test_get_type_high_card() {
        let cards: [Card;5] = [Card::Eight,Card::Ten,Card::Five,Card::Two,Card::Three];
        assert_eq!(Type::HighCard, get_type(&cards));
    }
#[test]
    fn test_jocker() {
        let a: [Card;5] = [Card::Jocker,Card::Jocker,Card::Jocker,Card::Jocker,Card::Jocker];
        let b: [Card;5] = [Card::Jocker,Card::Two,Card::Ace,Card::Ace,Card::Jocker];
        let c: [Card;5] = [Card::Ace,Card::Ace,Card::Two,Card::Ace,Card::Jocker];
        let d: [Card;5] = [Card::Ace,Card::Ace,Card::Ten,Card::Ten,Card::Jocker];
        assert_eq!(Type::FiveOfAKind, get_type(&a));
        assert_eq!(Type::FourOfAKind, get_type(&b));
        assert_eq!(Type::FourOfAKind, get_type(&c));
        assert_eq!(Type::FullHouse, get_type(&d));
    }
#[test]
    fn test_type_comparison() {
        assert!(Type::FiveOfAKind == Type::FiveOfAKind);
        assert!(Type::FiveOfAKind > Type::FourOfAKind);
        assert!(Type::FiveOfAKind > Type::FullHouse);
        assert!(Type::FiveOfAKind > Type::ThreeOfAKind);
        assert!(Type::FiveOfAKind > Type::TwoPairs);
        assert!(Type::FiveOfAKind > Type::OnePair);
        assert!(Type::FiveOfAKind > Type::HighCard);

        assert!(Type::FourOfAKind == Type::FourOfAKind);
        assert!(Type::FourOfAKind > Type::FullHouse);
        assert!(Type::FourOfAKind > Type::ThreeOfAKind);
        assert!(Type::FourOfAKind > Type::TwoPairs);
        assert!(Type::FourOfAKind > Type::OnePair);
        assert!(Type::FourOfAKind > Type::HighCard);

        assert!(Type::FullHouse == Type::FullHouse);
        assert!(Type::FullHouse > Type::ThreeOfAKind);
        assert!(Type::FullHouse > Type::TwoPairs);
        assert!(Type::FullHouse > Type::OnePair);
        assert!(Type::FullHouse > Type::HighCard);

        assert!(Type::ThreeOfAKind == Type::ThreeOfAKind);
        assert!(Type::ThreeOfAKind > Type::TwoPairs);
        assert!(Type::ThreeOfAKind > Type::OnePair);
        assert!(Type::ThreeOfAKind > Type::HighCard);

        assert!(Type::TwoPairs == Type::TwoPairs);
        assert!(Type::TwoPairs > Type::OnePair);
        assert!(Type::TwoPairs > Type::HighCard);

        assert!(Type::OnePair == Type::OnePair);
        assert!(Type::OnePair > Type::HighCard);

        assert!(Type::HighCard == Type::HighCard);
    }
}
