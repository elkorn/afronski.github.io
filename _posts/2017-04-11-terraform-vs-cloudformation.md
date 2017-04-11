---
layout: post
title: CloudFormation vs. Terraform
date: 2017-04-11T10:00+0200
tags:
  - devops
  - tools
  - aws
  - cloudformation
  - terraform
---

# *CloudFormation* vs. *Terraform*.

![CloudFormation vs. Terraform - fight!](/assets/CFvsTF.png)

There is a time when every project that uses cloud computing service has a difficult choice to make - should we automate infrastructure or not?

Immediately after another question pops up - how we should do it? Should we use a dedicated tool (*AWS CloudFormation*, *Azure Resource Templates*, *OpenStack Heat* or *Google Cloud Deployment Manager*) or a provider-agnostic solution? And then - immediately after a surprise comes in - at first glance, there is no other tool like the *Terraform* available on the market (but there is - it is called [Foreman](https://theforeman.org) :wink:). So we have a good tool, built by amazing people (*HashiCorp*) which do not rely on any particular *cloud* - problem solved? Not entirely.

As a grown up, you know that like the *ORM* does not let you change database on the fly, *Terraform* will not let you automatically switch cloud provider for your entire system (assuming no upfront investment into design and architect your system). From the other hand - you are afraid of *vendor lock-in*, and for sure you are considering how to design and prepare disaster recovery scenarios. A lot of unknowns, isn't it?

I would like to show our rationale for new project and infrastructure for which we were responsible. That motivation which driven us into pure *CloudFormation* setup, why we bet on *AWS*, and why we decided to drop *Terraform* assuming that certain conditions will stay with us for a long time.

Please have in mind that all remarks and comments pointed in this article are constructive criticism - it does not change my opinion about companies that created those tools at all. I have huge respect for both for *AWS* and *HashiCorp* - work they have done, especially in tooling and cloud computing landscape is outstanding. As a user of *AWS Services* and *HashiCorp* tools (mostly `Packer`, `Vagrant`, `Vault` and `Consul`) I am grateful for the work they did.

If you do not have an experience with *CloudFormation* or *Terraform* - please read either amazing documentation or any other article, that will introduce you to the topic - because I will rely on your basic knowledge about those two.

## Terraform is not a silver bullet.

When we evaluated *Terraform* it was before `0.7` release. According to the definition it is:

<quote class="citation">... a tool for building, changing, and versioning infrastructure safely and efficiently. Terraform can manage existing and popular service providers as well as custom in-house solutions.</quote>

It is tool built by practitioners, for fulfilling requirements of *infrastructure as a code* approach. It is built as an orchestration tool, focused on visibility (execution plans and resource graphs) and change automation, with minimal human interaction.

Sounds great, and what is even more important it is *cloud* / *provider* agnostic by design. That is a huge plus, especially when you need to consider either a hybrid cloud environment (mixing public cloud with on premises) or multiple cloud providers scenario (e.g. disaster recovery or availability requirements). What is also very important it does not focus on cloud *APIs* only - it incorporates various 3rd party APIs and Cloud provider API in one place, enabling interesting scenarios - e.g. creating infrastructure inside cloud provider, connecting it with your storage solution on premises and combining it with an external *DNS* provider.

What is often mentioned as a benefit is its *DSL* - *HashiCorp Configuration Language (HCL)*. In my opinion it is not a revolution, but evolution in right direction. Still declarative, but more expressive and at the same time more concise, than verbose and hostile *JSON* / *YAML* formats. *Terraform* internally is compatible with *JSON* format. I would not like to dive into details, because they are extensively covered in really good documentation: https://www.terraform.io/docs/configuration/syntax.html. One thing I want to point out - it is not a valid programming language, and we will talk about it soon as well.

So far so good - tool looked awesome for us. But before final decision I wanted to check its state and how it "feels". And that actually that shown a whole new perspective of it. First of all - from our perspective that tool is not as mature as we would like to see it. It is in the infancy stage - I have scanned the *Github* repository just for bugs related with *AWS* provider and it uncovered a long list. It is not a definite list (probably many of them was not confirmed, neither triaged / prioritized), but gives an answer how rapidly tool evolves. Second thing - which did not affect us directly, but we heard horror stories - there are some backward incompatible changes from version to version. Again - it is totally understandable, taking into account that tool is below 1.0, still it is not suitable for our needs - we wanted stable and tested solution, that will cover *AWS Services* for us (so cloud agnostic does not add anything for us, we discuss this later).

What stroke me the most is how it works underneath. It does not use *CloudFormation*, but *AWS API* - which has several consequences. One of it is impossibility to perform a rollback, when something will go crazy. Usual workflow is slightly different from the *CloudFormation* one- first we need to plan our changes, then we need to review them and decide - should we apply or not. With *CloudFormation* it is possible to review changes as well, but if everything goes hazy from any reason (and it eventually will - trust me :wink:) it will be able to roll-back that change and return to the previous state.

That workflow shows also one more disadvantage for us - *Terraform* is highly opinionated. It requires and assumes various thing regarding your workflow. For us it does not make a difference, but that tool is not used inside project, neither company in what we want to use it. It means that it adds additional impedance mismatch and requires effort regarding knowledge exchange and learning additional tool. Last, but not least - it is stateful. In the version we've evaluated there was no easy way to share and lock state in a remote environment, which was critical from our point of view (and neither storing state on *S3* or inside repository is not acceptable solution for us).

Up to that point we did not ditch *Terraform* yet, but we desperately searched for an alternative - we have definitely needed to face the *CloudFormation*.

## CloudFormation is not a hostile environment (neither a perfect one).

<section class="picture-section">
  <img alt="JSON or YAML - choice is yours, but choose wisely! ;)" src="/assets/Comparison.png" />
  <small>JSON or YAML - choice is yours, but choose wisely!</small>
</section>

From the other hand, inside the company where we wanted to built new infrastructure and *CloudFormation* was used across the *development* and *operations* departments. We heard horror stories too, and we wanted to be prepared for most of surprises before we start the project. So I have spent some time with that beast. And guess what? It is not as ugly as I initially thought.

The obvious advantage of that tool is its better support for *AWS* services than other 3rd party tools - no doubt on that. When *AWS* releases new service in most cases it is already supported inside *CloudFormation*, at least partially. However, there are some elements that either does not make sense for *CloudFormation* (e.g. registering *DNS* name for machine spawned inside auto-scaling group) or they are unsupported (e.g. *ACM Certificate Import).

According to the documentation - *CloudFormation* is stateless, but that is actually a tricky concept. It is stateless, except when it is not. :wink: From your perspective, you do not need to bother about state management, however it is not entirely true - it preserves stack inside the service, that stack resembles operations invoked inside your cloud (they called *events*) and it connects them with *resources*. Updates are based on top of that state. Another point are *exported outputs* which are globally shared inside the same region and *AWS* account. It means that you cannot create the same stack twice based on the declared definition, because it will collide for the defined outputs.

What about the learning curve? Well it turned out that it depends. Definitely it is not as hostile environment as it is advertised. If you will google for it - you will see how much people hate it. Of course, those people did not lie and in each accusation there is a grain of truth, but situation drastically changed after one announcement.

Announcement aforementioned above was related with releasing *YAML* format for the *CloudFormation* templates. Previously you could use only *JSON* for that. In my opinion it is a game changer. If you do not believe me, look at the screenshot posted above - difference is at least noticeable. I wrote templates in both formats and difference is huge, starting from really basic stuff like added support for comments (yes, *JSON* does not have comments at all), added support for multi-line strings (yay, no more string concatenation! This is really helpful for `"AWS::CloudFormation::Init"` sections and similar), to the smaller stuff like better support for invoking built-in functions. And last, but not least - it is less verbose and less painful to modify / refactor. In case of *JSON* it is really hard to refactor huge chunks of code, trying to make it readable and still preserving syntax validation (and *CloudFormation validation API* supports slightly malformed *JSON*, so you need to have two layers of checks).

Is it all? Not entirely. I have told you that there are couple of elements that are not supported directly in *CloudFormation*, which makes operation really painful and semi-manual. It turned out that you can either workaround it with some well-known hacks focusing e.g. on already mentioned (in different context) `"AWS::CloudFormation::Init"` or with new and strongly advertised concept that we wanted to test in practice - automating operations with *AWS Lambda* service. It turned out that with help of *SNS*, *SQS* and *AWS SDK* we were able to glue *CloudFormation* other services in an elegant way. In serverless and event-driven fashion.

<section class="picture-section">
  <img alt="Can you guess all the logos present in the picture?" src="/assets/GuessTheLogos.png" />
  <small>Can you guess all the logos present in the picture?</small>
</section>

## Vendor lock-in is a thing.

In this place I would like to stop and point out a real threat - *vendor lock-in is a thing*. It is not a myth, neither a boogeyman. Relying heavily on *CloudFormation* and adding on top of it a glue in form of *AWS Lambda* should be a deliberate choice. It is a considerable reliability, if you are considering moving out from the cloud.

If you are not a *Snapchat*, and do not have 2 billion USD for spending it in 5 years for a certain cloud provider, for sure you have considered what if we want to use a different provider or use physical infrastructure for our product. Moving either in or out between clouds or cloud and physical is a big project per se, sprinkling it with additional services provided by your cloud complicates it even more.

In most cases if you were careful, it will be rather a infrastructure and operations effort. However some services looks so shiny, so easy to use, so cheap (like *AWS Lambda*) and incorporating them looks like a really good choice. Except when you consider moving out from particular provider and you need to dedicate additional development and operations effort for such migration.

In our case decision it was actually simpler than you might think. We are moving out from physical data centers to the cloud of choice - *AWS* was chosen as a most mature one. There is no plan for migrating out of the cloud (we were not first project moving there, a huge development work was already done, including custom services on top of *AWS API*) and physical is not an elastic (obvious thing) and as cheap choice as you may think. Tests made in the cloud already confirmed, that even for our demanding domain (ad tech) that cloud provider will be sufficient for our needs. And it gives us more than physical world.

So you can consider it as a deliberate vendor lock-in, with all of its consequences. We were in the physical world, we know how it works and how it feels, and we still need more - that's why we've chosen the cloud. But it is not a usual path to take.

## *CloudFormation* pitfalls.

We have already scratched the surface in the previous paragraph, but speaking about *CloudFormation* - so far we are happy living with it, but it has sharp edges too. In that section I would like to point out couple of pitfalls, that knowing about may be helpful for you.

The most significant problem, covered also in many discussions over the internet, there are horror stories regarding updating and changesets for *CloudFormation*. We did not have any problems with an list of changes generated by service, that hides the actual operations - and simply not showing everything (there are people claiming that it is still a thing). However an update is harder than you think, in most cases because of replacements - it is hard to believe sometimes how simple change may enable domino effect, and will cause to rebuilt some unrelated elements. And killing elements is problematic.

Another topic already partially covered above are *exported outputs*. Those globals in the stateless world, are as much helpful, as annoying. From other hand, they are a neat way to connect stacks between, from the other - it is really easy to tangle stacks together and introduce dependencies, which are hard to reflect in the creation process.

Another element which is painful is naming (I will not repeat that old joke about hard things in computer science, but we all know it). Name should reflect the purpose, but also in many cases the location - for your convenience and better readability. In such case you have to remember which services are globally available, which are not (*Route53*, *IAM* and *S3*). And that is not the end of story, because for example - *S3* is a global service, and from the first glance you do not need to put region in the name of it. However bucket location makes a huge differences in scenario called cross-region replication, and having region name is really helpful. Decisions, decisions. Another pain-point related to naming are limits. Strangely enough they are not validated via *API*, only checked during the stack execution are various name length limitations (e.g. 64 characters limit for *ELB* name), you will know them by heart after couple of rounds with failures and rollbacks - just be prepared. :wink:

And last but not least - you want be always compliant with *CloudFormation* standards to start and stay small. Use always types provided by *CloudFormation*, which helps validation *API*. Prepare you own naming convention (also for tags) and stick to it as much as you can. Keep stacks as small as possible and split them in two dimensions - by responsibility (e.g. component) and by common modification reason (e.g. they are often modified together). Use exports with caution and wisely (naming convention also helps tremendously here).


## Summary

In our case, when a deliberate vendor lock-in was available as an option, we chosen *CloudFormation* and we are not regretting that choice so far. We were able to built our infrastructure and tools on top of solid and battle tested service, fully compliant with our cloud computing provider of choice. However I know that it is not an usual option for everyone - I hope that deepened approach to the topic and our rationale will help you to make right decision. Remember: there is no universal solution here, in case of any doubts or if you would like to hear more about certain topic feel free to reach me in the comments below or directly at my contact email.