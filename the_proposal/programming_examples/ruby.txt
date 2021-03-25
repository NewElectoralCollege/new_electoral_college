class Party
    @@list = []
    @@awarded = 0
    attr_reader :number, :seats

    def initialize(votes)
        @number = @@list.size + 1
        @votes = votes
        @seats = 0

        @@list.push(self)
    end

    def assign(quota)
        awarding = (@votes / quota).floor
        @seats += awarding
        @@awarded += awarding
    end

    def remainder(quota)
        @votes - (@seats * quota)
    end

    def extra_seat
        @seats += 1
    end

    def Party.list
        return @@list
    end

    def Party.awarded
        return @@awarded
    end
end

Party.new(400000)
Party.new(250000)
Party.new(100000)
Party.new(73000)
Party.new(5000)

total_votes = 828000
seats = 5
quota = (total_votes / seats).floor

Party.list.each do |n|
    n.assign(quota)
end

(Party.list.sort_by {|n| n.remainder(quota)}).reverse

Party.list[1..(seats - Party.awarded)].each do |n|
    n.extra_seat
end

Party.list.each do |n|
    puts "#{n.number}, #{n.seats} seats"
end