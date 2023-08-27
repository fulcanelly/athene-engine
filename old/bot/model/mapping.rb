class User < ActiveRecord::Base
    has_one :state

    has_many :actions
    has_many :some_items
end


class State < ActiveRecord::Base
    belongs_to :user 
end

class Action < ActiveRecord::Base
    belongs_to :user
end

