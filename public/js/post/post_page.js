import {CourseCategory, MultipleCourseCode, InquiryCategory} from 'es6!post/course_components';
import {SpecialistPost, MajorPost, MinorPost} from 'es6!post/post_components';

var CheckMyPost = React.createClass({

    componentDidMount: function() {
        var activeTab = this.refs.postNav.state.activeTab + 'Post';
        this.changeActiveTab(activeTab);
        this.updateNavCreditCounts();
    },

    changeActiveTab: function(newTab) {
        var activeTab = newTab;
        var tabs = ['spePost', 'majPost', 'minPost'];
        var me = this;

        tabs.forEach(function(tab) {
            if (tab === activeTab) {
                me.refs[tab].changeTabView(true);
            } else {
                me.refs[tab].changeTabView(false);
            }
        });
    },

    updateNavCreditCounts: function() {
        var newCounts = [this.refs.spePost.getCreditCount(),
                         this.refs.majPost.getCreditCount(),
                         this.refs.minPost.getCreditCount()];
        this.refs.postNav.setState({creditCounts: newCounts},
            this.updatePostStatus());
    },

    updatePostStatus: function() {
        var newStatuses = [this.refs.spePost.setIfCompleted(),
                           this.refs.majPost.setIfCompleted(),
                           this.refs.minPost.setIfCompleted()];
        this.refs.postNav.setState({completed: newStatuses});
    },

    render: function() {
        return (
            <div id='check_my_post'>
                <PostNav ref='postNav'
                         updateTab={this.changeActiveTab}
                         getCreditCountClass={this.getCreditCountClass} />
                <SpecialistPost ref='spePost' />
                <MajorPost ref='majPost' />
                <MinorPost ref='minPost' />
            </div>
        );
    }
});


var PostNav = React.createClass({
    getInitialState: function() {
        return {
            visible: getLocaStorage('activePost') === '' ? 'spe' : getLocaStorage('activePost'),
            creditCounts: [0.0, 0.0, 0.0],
            completed: [false, false, false],
            activeTab: getLocaStorage('activePost')
        }
    },

    getActiveTab: function() {
        return getLocaStorage('activePost');
    },

    changeActiveTab: function(e) {
        var newVisible = e.target.id.substring(0, 3);
        this.setState({visible: newVisible}, function() {
            this.props.updateTab(newVisible + 'Post');
            setLocaStorage('activePost', newVisible);
        });
    },

    getNavClass: function(type) {
        return this.state.visible === type ? 'nav_selected' : 'nav_not_selected';
    },

    getCreditCountClass: function(postIndex) {
        return this.state.completed[postIndex] ? 'credits_completed' : 'credits_not_completed';
    },

    render: function() {
        return (
            <nav id='posts'>
                <ul>
                    <li id='specialist' className={this.getNavClass('spe')}>
                        <a id='spec_link' onClick={this.changeActiveTab}> Specialist </a>
                        <div id='spec_creds' className={this.getCreditCountClass(0)}>
                            {'(' + this.state.creditCounts[0].toFixed(1) + '/12.0)'} </div>
                    </li>
                    <li id='major' className={this.getNavClass('maj')}>
                        <a id='maj_link' onClick={this.changeActiveTab}> Major </a>
                        <div id='maj_creds' className={this.getCreditCountClass(1)}>
                            {'(' + this.state.creditCounts[1].toFixed(1) + '/8.0)'} </div>
                    </li>
                    <li id='minor' className={this.getNavClass('min')}>
                        <a id='min_link' onClick={this.changeActiveTab}> Minor </a>
                        <div id='min_creds' className={this.getCreditCountClass(2)}>
                        {'(' + this.state.creditCounts[2].toFixed(1) + '/4.0)'} </div>
                    </li>
                </ul>
            </nav>
        );
    }

});


export default {CheckMyPost: CheckMyPost};
