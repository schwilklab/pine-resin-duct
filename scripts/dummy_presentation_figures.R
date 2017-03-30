figurez<- read.csv("../data/figurez.csv", stringsAsFactors=FALSE)

## Tradeoffs
# Positive Relationship
ggplot(figurez, aes(ring_width, postive)) +
  geom_smooth(method="lm", colour= "black") +
  xlab("Ring width") + ylab("Resin duct density") + labs(title = "Drought") +
  pubtheme.nogridlines +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(size=22))

# Negative Relationship
ggplot(figurez, aes(ring_width, negative)) +
  geom_smooth(method="lm", colour= "black") +
  xlab("Ring width") + ylab("Resin duct density") + labs(title = "Well-watered") +
  pubtheme.nogridlines +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(size=22))

## Age effects

# Positive Relationship
ggplot(figurez, aes(ring_width, postive)) +
  geom_smooth(method="lm", colour= "black") +
  xlab("Age") + ylab("Resin duct density") +
  pubtheme.nogridlines +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(size=22))

# Negative Relationship
ggplot(figurez, aes(ring_width, negative)) +
  geom_smooth(method="lm", colour= "black") +
  xlab("Age") + ylab("Ring width") +
  pubtheme.nogridlines +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(size=22))

## Tradeoffs with ring width
# Positive Relationship
ggplot(figurez, aes(ring_width, postive)) +
  geom_smooth(method="lm", colour= "black") +
  xlab("Ring width") + ylab("Resin duct density") +
  pubtheme.nogridlines +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(size=22))

# Negative Relationship
ggplot(figurez, aes(ring_width, negative)) +
  geom_smooth(method="lm", colour= "black") +
  xlab("Ring width") + ylab("Resin duct density") +
  pubtheme.nogridlines +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(size=22))

# Negative Relationship
ggplot(figurez, aes(ring_width, consistent)) +
  geom_smooth(method="lm", colour= "black") +
  xlab("Ring width") + ylab("Resin duct density") +
  pubtheme.nogridlines +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(size=22))
